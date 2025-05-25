(* mirage >= 4.9.0 & < 4.10.0 *)
(* Copyright Robur, 2020 *)

open Mirage

let dnsvizor =
  let pin = "git+file://" ^ Filename.dirname (Sys.getcwd ()) ^ "#HEAD" in
  let packages =
    [
      package ~pin "dnsvizor";
      package "logs";
      package "metrics";
      package "dns";
      package "dns-client";
      package "dns-mirage";
      package ~min:"10.1.0" ~sublibs:[ "mirage" ] "dns-resolver";
      package ~sublibs:[ "mirage" ] "dns-stub";
      package "hxd" ~sublibs:[ "core"; "string" ];
      package "dns-tsig";
      package "rresult";
      package "paf" ~sublibs:[ "mirage"; "alpn" ];
      package "dns-server";
      package ~min:"3.0.0" "ethernet";
      package ~min:"3.0.0" ~sublibs:[ "mirage" ] "arp";
      package ~min:"7.0.0"
        ~sublibs:[ "ipv4"; "tcp"; "udp"; "icmpv4"; "stack-direct"; "ipv6" ]
        "tcpip";
      package ~min:"1.6.0" "charrua";
      package "charrua-server";
      package ~min:"4.5.0" ~sublibs:[ "network" ] "mirage-runtime";
    ]
  in
  main ~packages "Unikernel.Main" (network @-> job)

(* this works around the [default_network] on Unix brings a --interface runtime
   argument that collides with dnsmasq arguments *)
let mynetwork =
  match_impl
    Key.(value target)
    [ (`Unix, netif ~group:"unix" "tap0") ]
    ~default:default_network

let () = register "dnsvizor" [ dnsvizor $ mynetwork ]
