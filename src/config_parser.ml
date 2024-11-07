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
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>= (fun dur ->
        match int_of_string_opt dur with
        | None -> fail (Fmt.str "Couldn't convert %S to an integer" dur)
        | Some n ->
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
                (Fmt.str "Value %u (from %S%s) does not fit into 32 bits" r dur
                   c))
  <|> string "infinite" *> return infinite

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

let pp_dhcp_range ppf
    { start_addr; end_addr; mode; netmask; broadcast; lease_time } =
  let pp_mode ppf = function
    | `Static -> Fmt.string ppf "static"
    | `Proxy -> Fmt.string ppf "proxy"
  in
  let pp_duration ppf = function
    | x when x = infinite -> Fmt.string ppf "infinite"
    | x when x mod week = 0 -> Fmt.pf ppf "%uw" (x / week)
    | x when x mod day = 0 -> Fmt.pf ppf "%ud" (x / day)
    | x when x mod hour = 0 -> Fmt.pf ppf "%uh" (x / hour)
    | x when x mod minute = 0 -> Fmt.pf ppf "%um" (x / minute)
    | x -> Fmt.pf ppf "%u" x
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

let mode =
  choice
    [ string "static" *> return `Static; string "proxy" *> return `Proxy ]
    ~failure_msg:"bad mode"

let dhcp_range =
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
  end_of_line <|> end_of_input >>| fun () ->
  let end_addr, mode =
    match mode_or_end with
    | `Mode m -> (None, Some m)
    | `End ip -> (Some ip, None)
  in
  let netmask, broadcast = net_broad in
  { start_addr; end_addr; mode; netmask; broadcast; lease_time }

let dhcp_range_docv =
  "<start>[,<end>|<mode>[,<netmask>[,<broadcast>]]][,<lease-time>]"

let dhcp_range_c =
  conv_cmdliner
    ~docv:dhcp_range_docv
    dhcp_range pp_dhcp_range

let parse_file data =
  let rules =
    let ignore_directive key =
      string (key ^ "=") *> commit *> ignore_line key >>| fun _ -> `Ignored
    in
    let ignore_flag key =
      string key *> (end_of_line <|> end_of_input) >>| fun _ -> `Ignored
    in
    let isspace = function
      | ' ' | '\x0c' | '\n' | '\r' | '\t' | '\x0b' -> true
      | _ -> false
    in
    skip_while isspace *> commit
    *> choice ~failure_msg:"bad configuration directive"
         [
           ( string "dhcp-range=" *> commit *> dhcp_range >>| fun range ->
             `Dhcp_range range );
           ignore_directive "interface";
           ignore_directive "except-interface";
           ignore_directive "listen-address";
           ignore_directive "no-dhcp-interface";
           ignore_flag "bind-interfaces";
           (string "#" *> ignore_line "#" >>| fun _ -> `Ignored);
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
