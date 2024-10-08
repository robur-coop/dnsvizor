(* mirage >= 4.7.0 & < 4.8.0 *)

(* Copyright Robur, 2020 *)

open Mirage

let dnsvizor =
  let packages = [
    package "logs" ;
    package "metrics" ;
    package "dns";
    package "dns-client";
    package "dns-mirage";
    package ~sublibs:["mirage"] "dns-resolver";
    package ~sublibs:["mirage"] "dns-stub";
    package "dns-tsig";
    package "dns-server";
    package ~min:"3.0.0" "ethernet";
    package ~min:"3.0.0" ~sublibs:["mirage"] "arp";
    package ~min:"7.0.0" ~sublibs:["ipv4"; "tcp"; "udp"; "icmpv4"; "stack-direct"; "ipv6"] "tcpip";
    package "charrua";
    package "charrua-server";
    package ~min:"4.5.0" ~sublibs:["network"] "mirage-runtime";
  ]
  and runtime_args = [
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv4";
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv4_gateway";
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv4_only";
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv6";
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv6_gateway";
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv6_only";
    runtime_arg ~pos:__POS__ "Unikernel.K.accept_router_advertisements";
    runtime_arg ~pos:__POS__ "Unikernel.K.dhcp_start";
    runtime_arg ~pos:__POS__ "Unikernel.K.dhcp_end";
    runtime_arg ~pos:__POS__ "Unikernel.K.dns_upstream";
    runtime_arg ~pos:__POS__ "Unikernel.K.dns_cache" ;
  ] in
  main
    ~runtime_args
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> network @-> job)

let () =
  register "dnsvizor" [
    dnsvizor $ default_random $ default_posix_clock
    $ default_monotonic_clock $ default_time
    $ default_network ]
