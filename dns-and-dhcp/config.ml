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

let upstream_resolver =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver IP" ["dns-upstream"] in
  Key.(create "dns-upstream" Arg.(opt (some ip_address) None doc))

let dnsvizor =
  let packages =
    [
      package "logs" ;
      package "metrics" ;
      package ~sublibs:["mirage"] "dns-stub";
      package "dns";
      package "dns-client";
      package "dns-mirage";
      package "dns-resolver";
      package "dns-tsig";
      package "dns-server";
      package "ethernet";
      package ~sublibs:["mirage"] "arp";
      package ~sublibs:["ipv4"; "tcp"; "udp"; "icmpv4"; "stack-direct"; "ipv6"] "tcpip";
      package "charrua";
      package "charrua-server";
    ]
  in
  foreign
    ~keys:[Key.abstract ipv4; Key.abstract ipv4_gateway; Key.abstract ipv4_only;
           Key.abstract ipv6; Key.abstract ipv6_gateway; Key.abstract ipv6_only;
           Key.abstract accept_router_advertisements;
           Key.abstract dhcp_start; Key.abstract dhcp_end;
           Key.abstract upstream_resolver]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> network @-> job)

let () =
  register "dnsvizor" [
    dnsvizor $ default_random $ default_posix_clock
    $ default_monotonic_clock $ default_time
    $ default_network ]
