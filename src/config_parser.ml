open Angstrom

module Log =
  (val Logs.(
         src_log
         @@ Src.create ~doc:"DNSvizor configuration module" "dnsvizor.config")
      : Logs.LOG)

let parse_one rule config_str =
  match parse_string ~consume:Consume.All rule config_str with
  | Ok _ as o -> o
  | Error msg -> Error (`Msg (Fmt.str "Parse error in %S: %s" config_str msg))

let conv_cmdliner ?docv rule pp = Cmdliner.Arg.conv ?docv (parse_one rule, pp)

(* some basic rules *)

let int =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun n ->
  match int_of_string_opt n with
  | None -> fail (Fmt.str "Couldn't convert %S to an integer" n)
  | Some n -> return n

(* be careful to not consume ',' anywhere - this is used as separator *)
let ipv4_dotted =
  take_while1 (function '0' .. '9' | '.' -> true | _ -> false) >>= fun ip ->
  match Ipaddr.V4.of_string ip with
  | Error (`Msg x) -> fail (Fmt.str "Invalid IPv4: %s: %S" x ip)
  | Ok ip -> return ip

let minute = 60
let hour = 60 * minute
let day = 24 * hour
let week = 7 * day
let infinite = 1 lsl 32 (* DHCP has 32 bits for this *)

let lease_time =
  int
  >>= (fun n ->
  choice ~failure_msg:"bad lease time"
    [
      string "w" *> return ("w", n * week);
      string "d" *> return ("d", n * day);
      string "h" *> return ("h", n * hour);
      string "m" *> return ("m", n * minute);
      return ("", n);
    ]
  >>= fun (c, r) ->
  if r > 0 && r < infinite then return r
  else
    fail
      (Fmt.str "Value %u (from %S%s) does not fit into 32 bits" r
         (string_of_int n) c))
  <|> string "infinite" *> return infinite

let isspace = function
  | ' ' | '\x0c' | '\n' | '\r' | '\t' | '\x0b' -> true
  | _ -> false

let conf_end_of_directive =
  let comment =
    char '#' *> commit
    *> skip_while (function '\n' -> false | _ -> true)
    *> end_of_line
    <|> end_of_input
  in
  fix (fun r ->
      end_of_line <|> end_of_input
      <|>
      (* A comment in a directive is only allowed if separated by a space *)
      skip isspace *> (comment <?> "comment" <|> r))
  <?> "config file end-of-directive"

let arg_end_of_directive = end_of_input

let line =
  take_while (function '\n' -> false | _ -> true)
  <* (end_of_line <|> end_of_input)

let ignore_line key =
  line >>= fun txt ->
  Log.warn (fun m -> m "ignoring %S %S" key txt);
  return txt

let pp_ignored_line key ppf data = Fmt.pf ppf "--%s=%s" key data

let ignore_c key =
  conv_cmdliner ~docv:"IGNORED" (ignore_line key) (pp_ignored_line key)

(* real grammars *)

type dhcp_range = {
  start_addr : Ipaddr.V4.t;
  end_addr : Ipaddr.V4.t option;
  mode : [ `Static | `Proxy ] option;
  netmask : Ipaddr.V4.t option;
  broadcast : Ipaddr.V4.t option;
  lease_time : int option;
}

type dhcp_host = {
  id : [ `Any_client_id | `Client_id of string ] option;
  sets : string list;
  tags : string list;
  macs : Macaddr.t list;
  ipv4 : Ipaddr.V4.t option;
  ipv6 : Ipaddr.V6.t option;
  lease_time : int option;
  ignore : bool;
  (* TODO: [`host] Domain_name.t?! *)
  domain_name : [ `raw ] Domain_name.t option;
}
(* the dhcp_host data structure is not great to work with. The fields [id],
   and [macs] are used for matching clients DHCPREQUEST. The [tags] field is
   matched internally to add more dhcp options in the DHCPREPLY. The fields
   [ipv4] and [lease_time] are values to assign to matching clients. The [sets]
   field is used to internally assign tags to matching clients. The [ignore]
   field says to ignore matching clients making the [ipv4] and [lease_time]
   fields questionable. *)

let pp_duration ppf = function
  | x when x = infinite -> Fmt.string ppf "infinite"
  | x when x mod week = 0 -> Fmt.pf ppf "%uw" (x / week)
  | x when x mod day = 0 -> Fmt.pf ppf "%ud" (x / day)
  | x when x mod hour = 0 -> Fmt.pf ppf "%uh" (x / hour)
  | x when x mod minute = 0 -> Fmt.pf ppf "%um" (x / minute)
  | x -> Fmt.pf ppf "%u" x

let pp_dhcp_range ppf
    { start_addr; end_addr; mode; netmask; broadcast; lease_time } =
  let pp_mode ppf = function
    | `Static -> Fmt.string ppf "static"
    | `Proxy -> Fmt.string ppf "proxy"
  in
  Fmt.pf ppf "%a%a%a%a%a%a" Ipaddr.V4.pp start_addr
    Fmt.(option ~none:nop (any "," ++ Ipaddr.V4.pp))
    end_addr
    Fmt.(option ~none:nop (any "," ++ pp_mode))
    mode
    Fmt.(option ~none:nop (any "," ++ Ipaddr.V4.pp))
    netmask
    Fmt.(option ~none:nop (any "," ++ Ipaddr.V4.pp))
    broadcast
    Fmt.(option ~none:nop (any "," ++ pp_duration))
    lease_time

let pp_dhcp_host ppf
    { id; sets; tags; macs; ipv4; ipv6; lease_time; ignore; domain_name } =
  let sep =
    let use_sep = ref false in
    fun () ->
      if !use_sep then Fmt.pf ppf ",";
      use_sep := true
  in
  List.iter
    (fun mac ->
      sep ();
      Macaddr.pp ppf mac)
    macs;
  Option.iter
    (fun id ->
      sep ();
      match id with
      | `Any_client_id -> Fmt.pf ppf "id:*"
      | `Client_id id ->
          (* TODO: use hex when text is inappropriate *)
          Fmt.pf ppf "id:%s" id)
    id;
  List.iter
    (fun set ->
      sep ();
      Fmt.pf ppf "set:%s" set)
    sets;
  List.iter
    (fun tag ->
      sep ();
      Fmt.pf ppf "tag:%s" tag)
    tags;
  Option.iter
    (fun ipv4 ->
      sep ();
      Ipaddr.V4.pp ppf ipv4)
    ipv4;
  Option.iter
    (fun ipv6 ->
      sep ();
      Ipaddr.V6.pp ppf ipv6)
    ipv6;
  Option.iter
    (fun domain_name ->
      sep ();
      Domain_name.pp ppf domain_name)
    domain_name;
  Option.iter
    (fun lease_time ->
      sep ();
      pp_duration ppf lease_time)
    lease_time;
  if ignore then (
    sep ();
    Fmt.pf ppf "ignore")

let mode =
  choice
    [ string "static" *> return `Static; string "proxy" *> return `Proxy ]
    ~failure_msg:"bad mode"

let dhcp_range end_of_directive =
  (* TODO prefix: [tag:<tag>[,tag:<tag>],][set:<tag>,]
     V4: <start-addr>[,<end-addr>|<mode>[,<netmask>[,<broadcast>]]][,<lease time>]
     TODO V6: <start-IPv6addr>[,<end-IPv6addr>|constructor:<interface>][,<mode>][,<prefix-len>][,<lease time>] *)
  (* TODO mode can be static (dhcp-host), proxy (pxe-prompt/pxe-server),
     IPv6: ra-only, slaac, ra-names, ra-stateless, ra-advrouter, off-link
  *)
  ipv4_dotted >>= fun start_addr ->
  string ","
  *> (ipv4_dotted >>| (fun ip -> `End ip) <|> (mode >>| fun mode -> `Mode mode))
  >>= fun mode_or_end ->
  option (None, None)
    ( (string "," *> ipv4_dotted >>| fun ip -> Some ip) >>= fun netmask ->
      option None (string "," *> ipv4_dotted >>| fun ip -> Some ip)
      >>| fun broadcast -> (netmask, broadcast) )
  >>= fun net_broad ->
  option None (string "," *> lease_time >>| fun l -> Some l)
  >>= fun lease_time ->
  end_of_directive >>| fun () ->
  let end_addr, mode =
    match mode_or_end with
    | `Mode m -> (None, Some m)
    | `End ip -> (Some ip, None)
  in
  let netmask, broadcast = net_broad in
  { start_addr; end_addr; mode; netmask; broadcast; lease_time }

let until_comma =
  scan_string () (fun () c ->
      (* FIXME: probably be more precise in accepted characters *)
      match c with
      | ',' -> None
      | _ -> Some ())

let tag_thing = string "tag:" *> commit *> until_comma

let dhcp_host end_of_directive =
  let lease_time = lease_time >>| fun lease -> `Lease_time lease in
  let id_thing =
    string_ci "id:" *> commit
    *> choice ~failure_msg:"Bad id thing"
         [
           char '*' *> return `Any_client_id;
           (* FIXME: probably be more precise in accepted characters *)
           ( scan false (fun is_hex -> function
               | ',' -> None
               | ':' -> Some true
               | _ -> Some is_hex)
           <* commit
           >>= fun (name, is_hex) ->
             if is_hex then
               (* This is not very smart *)
               let hex_name =
                 String.concat "" (String.split_on_char ':' name)
               in
               match Ohex.decode hex_name with
               | name -> return (`Client_id name)
               | exception Invalid_argument e ->
                   fail (Fmt.str "bad hex constant: %s: %S" e name)
             else return (`Client_id name) );
         ]
  in
  let net_set_thing =
    choice
      [
        string "net:" *> commit
        *> fail "Using 'net:' is unsupported; use 'set:' instead.";
        string "set:";
      ]
    *> commit *> until_comma
    >>| fun set -> `Set set
  in
  let mac_addr =
    (* NOTE: ocaml-tcpip only supports mac addresses of 6 bytes so let's not
       try to parse mac addresses for exotic hardware. We will allow ethernet
       (10 Mb) mac type only. *)
    (* TODO: wildcards *)
    let ishex = function
      | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
      | _ -> false
    in
    option "" (string "1-") *> peek_string 3 >>= fun first ->
    if ishex first.[0] && ishex first.[1] && first.[2] = ':' then
      commit *> take ((6 * 2) + 5) >>= fun mac ->
      match Macaddr.of_string mac with
      | Ok mac -> return (`Macaddr mac)
      | Error (`Msg e) -> fail (Fmt.str "Invalid MAC address: %s: %S" e mac)
    else fail "not a mac address"
  in
  let ipv4_addr = ipv4_dotted >>| fun ip -> `Ipv4addr ip in
  let ignore_thing = string "ignore" *> return `Ignore in
  let hostname =
    sep_by1 (char '.')
      (scan_string () (fun () c ->
           match c with
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> Some ()
           | _ -> None))
    >>= fun labels ->
    (* TODO: refine domain name kind *)
    match Domain_name.of_strings labels with
    | Ok domain -> return (`Domain_name domain)
    | Error (`Msg e) ->
        fail
          (Fmt.str "Invalid domain name: %s: %a" e
             Fmt.(list ~sep:(any ".") string)
             labels)
  in
  let dhcp_host_item =
    choice ~failure_msg:"Bad dhcp-host argument"
      [
        id_thing;
        net_set_thing;
        (tag_thing >>| fun tag -> `Tag tag);
        mac_addr;
        ipv4_addr;
        (* TODO: ipv6_addr ; *)
        lease_time;
        ignore_thing;
        hostname;
      ]
  in
  sep_by1 (char ',') dhcp_host_item <* end_of_directive >>= fun items ->
  (* Process items:
     - We can have at most one id thing except and id and id:* whose semantics
       are very unclear. Thus we should probably forbid that combination.
     - For net:/set: we can have as many as we like.
     - We can as well have as many tag: as we like.
     - There is also no limit on mac addresses.
     - There can be at most one ipv4 address.
     - There can be at most one lease time.
     - The ignore thing will set a flag. Repeating it is redundant if harmless.
     - There can be at most one domain name.
     The option parser in dnsmasq will not enforce much of this. Instead, the
     last value will overwrite previous values if only one value makes sense.
     We should do better. *)
  let exception Duplicate_item of string in
  match
    List.fold_left
      (fun config item ->
        match item with
        | (`Any_client_id | `Client_id _) as id ->
            if Option.is_some config.id then
              raise_notrace (Duplicate_item "id:<client_id>");
            { config with id = Some id }
        | `Set set -> { config with sets = set :: config.sets }
        | `Tag tag -> { config with tags = tag :: config.tags }
        | `Macaddr mac -> { config with macs = mac :: config.macs }
        | `Ipv4addr ipv4 ->
            if Option.is_some config.ipv4 then
              raise_notrace (Duplicate_item "<ipv4>");
            { config with ipv4 = Some ipv4 }
        | `Lease_time time ->
            if Option.is_some config.lease_time then
              raise_notrace (Duplicate_item "<lease_time>");
            { config with lease_time = Some time }
        | `Ignore ->
            if config.ignore then
              Log.warn (fun m -> m "Redundant 'ignore' in dhcp-host.");
            { config with ignore = true }
        | `Domain_name domain_name ->
            if Option.is_some config.domain_name then
              raise_notrace (Duplicate_item "<host_name>");
            { config with domain_name = Some domain_name })
      {
        id = None;
        sets = [];
        tags = [];
        macs = [];
        ipv4 = None;
        ipv6 = None;
        lease_time = None;
        ignore = false;
        domain_name = None;
      }
      items
  with
  | thing ->
      (* Above we reverse the order so let's undo that. *)
      let thing =
        {
          thing with
          sets = List.rev thing.sets;
          tags = List.rev thing.tags;
          macs = List.rev thing.macs;
        }
      in
      return thing
  | exception Duplicate_item what ->
      Fmt.kstr fail
        "Duplicate argument %s in dhcp-host. Dnsmasq will accept this and \
         *silently* ignore any previous values. Remove earlier %s argument(s)."
        what what

type address_range =
  [ `Ip_range of Ipaddr.V4.t * Ipaddr.V4.t | `Ip of Ipaddr.V4.Prefix.t ]

let address_range =
  ipv4_dotted >>= fun start ->
  string "," *> ipv4_dotted
  >>= (fun stop -> return (`Ip_range (start, stop)))
  <|> ( string "/" *> int >>= fun prefix ->
        return (`Ip (Ipaddr.V4.Prefix.make prefix start)) )

type dhcp_option = { tags : string list; option : Dhcp_wire.dhcp_option }

let dhcp_opt_code =
  let integer_opt =
    int >>= fun v ->
    if v < 0 || v >= 256 then fail "Invalid option number"
    else
      match Dhcp_wire.int_to_option_code v with
      | None -> fail "Invalid option number"
      | Some option_code -> return option_code
  in
  let log_server = string "log-server" *> return Dhcp_wire.LOG_SERVERS in
  choice ~failure_msg:"option:" [ integer_opt; log_server ]

let dhcp_opt =
  dhcp_opt_code <* commit >>= function
  | Dhcp_wire.LOG_SERVERS ->
      Log.err (fun m -> m "LOG_SERVERS");
      many1 (char ',' *> ipv4_dotted) <?> "log-servers ips"
      >>= fun log_servers -> return (Dhcp_wire.Log_servers log_servers)
  | code ->
      Format.kasprintf fail "Unsupported dhcp option %s"
        (Dhcp_wire.option_code_to_string code)

let dhcp_option end_of_directive =
  (* [tag:<tag>,[tag:<tag>,]][encap:<opt>,][vi-encap:<enterprise>,][vendor:[<vendor-class>],][<opt>|option:<opt-name>|option6:<opt>|option6:<opt-name>],[<value>[,<value>]] *)
  many (tag_thing <* char ',') >>= fun tags ->
  string "option:" *> commit *> dhcp_opt <* end_of_directive >>| fun option ->
  { option; tags }

let pp_dhcp_opt ppf = function
  | Dhcp_wire.Log_servers log_servers ->
      Fmt.pf ppf "log-servers,%a"
        Fmt.(list ~sep:(any ",") Ipaddr.V4.pp)
        log_servers
  | _ -> assert false

let pp_dhcp_option ppf { tags; option } =
  Fmt.pf ppf "%aoption:%a"
    Fmt.(list ~sep:nop (string ++ any ","))
    tags pp_dhcp_opt option

let pp_address_range ppf = function
  | `Ip_range (a, b) -> Fmt.pf ppf "%a,%a" Ipaddr.V4.pp a Ipaddr.V4.pp b
  | `Ip pre -> Ipaddr.V4.Prefix.pp ppf pre

type domain =
  [ `raw ] Domain_name.t
  * [ `Interface of string
    | `Ip of Ipaddr.V4.Prefix.t
    | `Ip_range of Ipaddr.V4.t * Ipaddr.V4.t ]
    option

let pp_domain ppf (domain, ip_or_interface) =
  Fmt.pf ppf "%a%a" Domain_name.pp domain
    Fmt.(option ~none:(any "") string)
    (Option.map
       (function
         | `Interface int -> "," ^ int
         | #address_range as ar -> "," ^ Fmt.to_to_string pp_address_range ar)
       ip_or_interface)

let domain end_of_directive =
  ( take_while1 (function ',' -> false | _ -> true) >>= fun name ->
    match Domain_name.of_string name with
    | Ok domain -> return domain
    | Error (`Msg e) -> fail (Fmt.str "Invalid domain: %s: %S" e name) )
  >>= fun domain ->
  option None
    (string ","
    *> (address_range
       >>= (fun ar -> return (Some ar))
       <|> ( take_while (function '\n' -> false | _ -> true) >>= fun intf ->
             return (Some (`Interface intf)) )))
  >>= fun snd ->
  end_of_directive >>| fun () -> (domain, snd)

let dhcp_range_docv =
  "<start>[,<end>|<mode>[,<netmask>[,<broadcast>]]][,<lease-time>]"

let dhcp_range_c =
  conv_cmdliner ~docv:dhcp_range_docv
    (dhcp_range arg_end_of_directive)
    pp_dhcp_range

let dhcp_host_docv =
  "[<hwaddr>][,id:<client_id>|*][,set:<tag>][,tag:<tag>][,<ipaddr>][,<host‐name>][,<lease_time>][,ignore]"

let dhcp_host_c =
  conv_cmdliner ~docv:dhcp_range_docv
    (dhcp_host arg_end_of_directive)
    pp_dhcp_host

let dhcp_option_docv =
  "[tag:<tag>,[tag:<tag>,]][opt|option:<opt-name>],[<value>[,value]]"

let dhcp_option_c =
  conv_cmdliner ~docv:dhcp_option_docv
    (dhcp_option arg_end_of_directive)
    pp_dhcp_option

let domain_docv =
  (* note that address range is either "<start address>,<end address>" or "<ip address>/<prefix length>" *)
  (* TODO dnsmasq has [,local] after the "<address range>" *)
  "<domain>[[,<address range>]|<interface>]"

let domain_c =
  conv_cmdliner ~docv:domain_docv (domain arg_end_of_directive) pp_domain

type config_item =
  [ `Dhcp_range of dhcp_range
  | `Dhcp_host of dhcp_host
  | `Domain of domain
  | `Dhcp_option of dhcp_option
  | `No_hosts
  | `Dnssec ]

type config = [ config_item | `Ignored ] list

let pp_config_item mode ppf item =
  let pfx = match mode with `File -> Fmt.nop | `Arg -> Fmt.any "--" in
  pfx ppf ();
  match item with
  | `Dhcp_range dhcp_range ->
      Fmt.pf ppf "dhcp-range=%a" pp_dhcp_range dhcp_range
  | `Dhcp_host dhcp_host -> Fmt.pf ppf "dhcp-host=%a" pp_dhcp_host dhcp_host
  | `Domain domain -> Fmt.pf ppf "domain=%a" pp_domain domain
  | `Dhcp_option dhcp_option ->
      Fmt.pf ppf "dhcp-option=%a" pp_dhcp_option dhcp_option
  | `No_hosts -> Fmt.string ppf "no-hosts"
  | `Dnssec -> Fmt.string ppf "dnssec"

let pp_config mode ppf config =
  let sep = match mode with `File -> Fmt.any "\n" | `Arg -> Fmt.any " " in
  Fmt.(list ~sep)
    (pp_config_item mode) ppf
    (List.filter_map
       (function `Ignored -> None | #config_item as item -> Some item)
       config)

let parse_file data =
  let rules =
    let skip_spcs = skip_while (function ' ' -> true | _ -> false) in
    let directive_prefix name =
      string name *> skip_spcs *> char '=' *> commit *> skip_spcs
      <?> Printf.sprintf "directive prefix %S" name
    in
    let flag name = string name *> conf_end_of_directive in
    let ignore_directive key =
      directive_prefix key *> commit *> ignore_line key >>| fun _ -> `Ignored
    in
    let ignore_flag key =
      string key *> conf_end_of_directive >>| fun _ -> `Ignored
    in
    skip_while isspace *> commit
    *> choice ~failure_msg:"bad configuration directive"
         [
           ( directive_prefix "dhcp-range" *> dhcp_range conf_end_of_directive
           >>| fun range -> `Dhcp_range range );
           (flag "dnssec" >>| fun _ -> `Dnssec);
           ( directive_prefix "domain" *> domain conf_end_of_directive
           >>| fun domain -> `Domain domain );
           ( directive_prefix "dhcp-option" *> dhcp_option conf_end_of_directive
           >>| fun dhcp_option -> `Dhcp_option dhcp_option );
           ignore_directive "interface";
           ignore_directive "except-interface";
           ignore_directive "listen-address";
           ignore_directive "no-dhcp-interface";
           ignore_flag "bind-interfaces";
           ( string "#" *> commit *> ignore_line "#" <?> "comment" >>| fun _ ->
             `Ignored );
         ]
  in
  let top =
    fix (fun r ->
        rules >>= fun e ->
        commit *> end_of_input *> return [ e ] <|> (List.cons e <$> r))
  in
  match parse_string ~consume:Consume.All top data with
  | Ok x -> Ok (List.filter (function `Ignored -> false | _ -> true) x)
  | Error msg -> Error (`Msg msg)
