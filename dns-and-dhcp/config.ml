(* Copyright Robur, 2020 *)

open Mirage

let ipv4 = Key.V4.network Ipaddr.V4.Prefix.loopback

let ipv4_gateway = Key.V4.gateway None

let ipv4_only = Key.ipv4_only ()

let ipv6 = Key.V6.network None

let ipv6_gateway = Key.V6.gateway None

let ipv6_only = Key.ipv6_only ()

let accept_router_advertisements = Key.V6.accept_router_advertisements ()

let dhcp_start =
  let doc =
    Key.Arg.info ~doc:"DHCP range start (defaults to .100 if ipv4 is a /24)"
      ["dhcp-start"]
  in
  Key.(create "dhcp-start" Arg.(opt (some ipv4_address) None doc))

let dhcp_end =
  let doc =
    Key.Arg.info ~doc:"DHCP range end (defaults to .254 if ipv4 is a /24)"
      ["dhcp-end"]
  in
  Key.(create "dhcp-end" Arg.(opt (some ipv4_address) None doc))

let dns_upstream =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver" ["dns-upstream"] in
  Key.(create "dns-upstream" Arg.(opt (some string) None doc))

let dnsvizor =
  let packages =
    [
      package "logs" ;
      package "metrics" ;
      package ~min:"6.4.0" ~sublibs:["mirage"] "dns-stub";
      package "dns";
      package "dns-client";
      package "dns-mirage";
      package "dns-resolver";
      package "dns-tsig";
      package "dns-server";
      package ~min:"3.0.0" "ethernet";
      package ~min:"3.0.0" ~sublibs:["mirage"] "arp";
      package ~min:"7.0.0" ~sublibs:["ipv4"; "tcp"; "udp"; "icmpv4"; "stack-direct"; "ipv6"] "tcpip";
      package "charrua";
      package "charrua-server";
      package ~min:"4.3.1" "mirage-runtime";
    ]
  in
  foreign
    ~keys:[Key.v ipv4; Key.v ipv4_gateway; Key.v ipv4_only;
           Key.v ipv6; Key.v ipv6_gateway; Key.v ipv6_only;
           Key.v accept_router_advertisements;
           Key.v dhcp_start; Key.v dhcp_end;
           Key.v dns_upstream]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> network @-> job)

let () =
  register "dnsvizor" [
    dnsvizor $ default_random $ default_posix_clock
    $ default_monotonic_clock $ default_time
    $ default_network ]
