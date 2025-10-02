(* mirage >= 4.10.0 & < 4.11.0 *)
(* Copyright Robur, 2020 *)

open Mirage

let assets = crunch "assets"

let dnsvizor =
  let local_libs = [ "dnsvizor" ] in
  let packages =
    [
      package "logs";
      package ~min:"0.5.0" "metrics";
      package "dns";
      package "dns-client";
      package "dns-client-mirage";
      package "dnssec";
      package "dns-mirage";
      package ~min:"10.2.0" ~sublibs:[ "mirage" ] "dns-resolver";
      package ~sublibs:[ "mirage" ] "dns-stub";
      package "dns-tsig";
      package "dns-server";
      package "dns";
      package "paf" ~sublibs:[ "mirage"; "alpn" ];
      package ~min:"3.0.0" "ethernet";
      package ~min:"3.0.0" ~sublibs:[ "mirage" ] "arp";
      package ~min:"7.0.0"
        ~sublibs:[ "ipv4"; "tcp"; "udp"; "icmpv4"; "stack-direct"; "ipv6" ]
        "tcpip";
      package ~min:"2.1.0" "charrua";
      package "charrua-server";
      package ~min:"4.5.0" ~sublibs:[ "network" ] "mirage-runtime";
      package "tyxml";
      package "http-mirage-client";
      package "angstrom";
      package "multipart_form";
    ]
  in
  main ~local_libs ~packages "Unikernel.Main" (network @-> kv_ro @-> job)

(* this works around the [default_network] on Unix brings a --interface runtime
   argument that collides with dnsmasq arguments *)
let mynetwork =
  match_impl
    Key.(value target)
    [ (`Unix, netif ~group:"unix" "tap0") ]
    ~default:default_network

let enable_monitoring =
  let doc =
    Key.Arg.info
      ~doc:
        "Enable monitoring (syslog, metrics to influx, log level, statmemprof \
         tracing)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management"
       (netif ~group:"management" "management"))
    (generic_stackv4v6 mynetwork)

let monitoring =
  let monitor = Runtime_arg.(v (monitor None)) in
  let connect _ modname = function
    | [ stack; monitor ] ->
        code ~pos:__POS__
          "Lwt.return (match %s with| None -> Logs.warn (fun m -> m \"no \
           monitor specified, not outputting statistics\")| Some ip -> \
           %s.create ip ~hostname:(Mirage_runtime.name ()) %s)"
          monitor modname stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~min:"0.0.6" "mirage-monitoring" ]
    ~runtime_args:[ monitor ] ~connect "Mirage_monitoring.Make"
    (stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_arg.(v (syslog None)) in
  let connect _ modname = function
    | [ stack; syslog ] ->
        code ~pos:__POS__
          "Lwt.return (match %s with| None -> Logs.warn (fun m -> m \"no \
           syslog specified, dumping on stdout\")| Some ip -> \
           Logs.set_reporter (%s.create %s ip ~hostname:(Mirage_runtime.name \
           ()) ()))"
          syslog modname stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:[ "mirage" ] ~min:"0.5.0" "logs-syslog" ]
    ~runtime_args:[ syslog ] ~connect "Logs_syslog_mirage.Udp"
    (stackv4v6 @-> job)

let optional_monitoring stack =
  if_impl (Key.value enable_monitoring) (monitoring $ stack) noop

let optional_syslog stack =
  if_impl (Key.value enable_monitoring) (syslog $ stack) noop

let () =
  register "dnsvizor"
    [
      optional_syslog management_stack;
      optional_monitoring management_stack;
      dnsvizor $ mynetwork $ assets;
    ]
