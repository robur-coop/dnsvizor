open Angstrom

let parse_one rule config_str =
  parse_string ~consume:Consume.All
    (rule
    <|> ( available >>| min 100 >>= peek_string >>= fun context ->
          pos >>= fun pos ->
          fail (Printf.sprintf "Error at byte offset %d: %S" pos context) ))
    config_str

let lift_err = function Ok _ as o -> o | Error e -> Error (`Msg e)

let conv_cmdliner ?docv rule pp =
  Cmdliner.Arg.conv ?docv ((fun x -> parse_one rule x |> lift_err), pp)

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
        | Some dur ->
            choice
              [
                string "w" *> return (dur * week);
                string "d" *> return (dur * day);
                string "h" *> return (dur * hour);
                string "m" *> return (dur * minute);
                end_of_input *> return dur;
              ])
  <|> string "infinite" *> return infinite

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
  choice [ string "static" *> return `Static; string "proxy" *> return `Proxy ]

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
  >>| fun lease_time ->
  let end_addr, mode =
    match mode_or_end with
    | `Mode m -> (None, Some m)
    | `End ip -> (Some ip, None)
  in
  let netmask, broadcast = net_broad in
  { start_addr; end_addr; mode; netmask; broadcast; lease_time }
(* lease time is in seconds, or minutes (45m) or hours (1h) or days (2d)
   or weeks (1w) or "infinite", default is 1h, minimum is 2m *)
(* examples:
   # Uncomment this to enable the integrated DHCP server, you need
   # to supply the range of addresses available for lease and optionally
   # a lease time. If you have more than one network, you will need to
   # repeat this for each network on which you want to supply DHCP
   # service.
   #dhcp-range=192.168.0.50,192.168.0.150,12h

   # This is an example of a DHCP range where the netmask is given. This
   # is needed for networks we reach the dnsmasq DHCP server via a relay
   # agent. If you don't know what a DHCP relay agent is, you probably
   # don't need to worry about this.
   #dhcp-range=192.168.0.50,192.168.0.150,255.255.255.0,12h

   # This is an example of a DHCP range which sets a tag, so that
   # some DHCP options may be set only for this network.
   #dhcp-range=set:red,192.168.0.50,192.168.0.150

   # Use this DHCP range only when the tag "green" is set.
   #dhcp-range=tag:green,192.168.0.50,192.168.0.150,12h

   # Specify a subnet which can't be used for dynamic address allocation,
   # is available for hosts with matching --dhcp-host lines. Note that
   # dhcp-host declarations will be ignored unless there is a dhcp-range
   # of some type for the subnet in question.
   # In this case the netmask is implied (it comes from the network
   # configuration on the machine running dnsmasq) it is possible to give
   # an explicit netmask instead.
   #dhcp-range=192.168.0.0,static

   # Enable DHCPv6. Note that the prefix-length does not need to be specified
   # and defaults to 64 if missing/
   #dhcp-range=1234::2, 1234::500, 64, 12h

   # Do Router Advertisements, BUT NOT DHCP for this subnet.
   #dhcp-range=1234::, ra-only

   # Do Router Advertisements, BUT NOT DHCP for this subnet, also try and
   # add names to the DNS for the IPv6 address of SLAAC-configured dual-stack
   # hosts. Use the DHCPv4 lease to derive the name, network segment and
   # MAC address and assume that the host will also have an
   # IPv6 address calculated using the SLAAC algorithm.
   #dhcp-range=1234::, ra-names

   # Do Router Advertisements, BUT NOT DHCP for this subnet.
   # Set the lifetime to 46 hours. (Note: minimum lifetime is 2 hours.)
   #dhcp-range=1234::, ra-only, 48h

   # Do DHCP and Router Advertisements for this subnet. Set the A bit in the RA
   # so that clients can use SLAAC addresses as well as DHCP ones.
   #dhcp-range=1234::2, 1234::500, slaac

   # Do Router Advertisements and stateless DHCP for this subnet. Clients will
   # not get addresses from DHCP, but they will get other configuration information.
   # They will use SLAAC for addresses.
   #dhcp-range=1234::, ra-stateless

   # Do stateless DHCP, SLAAC, and generate DNS names for SLAAC addresses
   # from DHCPv4 leases.
   #dhcp-range=1234::, ra-stateless, ra-names
*)

let dhcp_range_c =
  conv_cmdliner ~docv:"<start>[,<end>|<mode>[,<netmask>[,<broadcast>]]][,<lease-time>]" dhcp_range pp_dhcp_range
