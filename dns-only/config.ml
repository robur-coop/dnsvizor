(* Copyright Robur, 2020 *)

open Mirage

let dns_upstream =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver IP" ["dns-upstream"] in
  Key.(create "dns-upstream" Arg.(opt (some ip_address) None doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let tls_hostname =
  let doc = Key.Arg.info ~doc:"Hostname to use for TLS authentication" ["tls-hostname"] in
  Key.(create "tls-hostname" Arg.(opt (some string) None doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"TLS authenticator" ["authenticator"] in
  Key.(create "authenticator" Arg.(opt (some string) None doc))

let no_tls =
  let doc = Key.Arg.info ~doc:"Disable DNS-over-TLS" ["no-tls"] in
  Key.(create "no-tls" Arg.(opt bool false doc))

let dnsvizor =
  let packages =
    [
      package "logs" ;
      package "metrics" ;
      package ~min:"6.0.0" ~sublibs:["mirage"] "dns-stub";
      package "dns";
      package "dns-client";
      package "dns-mirage";
      package "dns-resolver";
      package "dns-tsig";
      package "dns-server";
      package "ca-certs-nss";
      package ~min:"0.16.0" "x509";
    ]
  in
  foreign
    ~keys:[Key.v dns_upstream ; Key.v dns_port ; Key.v tls_hostname ; Key.v authenticator ; Key.v no_tls]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> job)

let () =
  register "dns-stub" [
    dnsvizor $ default_random $ default_posix_clock
    $ default_monotonic_clock $ default_time
    $ generic_stackv4v6 default_network ]
