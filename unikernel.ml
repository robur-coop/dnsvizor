open Lwt.Infix

module CA = struct
  let prefix =
    X509.Distinguished_name.
      [ Relative_distinguished_name.singleton (CN "DNSvizor") ]

  let cacert_dn =
    let open X509.Distinguished_name in
    prefix
    @ [ Relative_distinguished_name.singleton (CN "Ephemeral CA for DNSvizor") ]

  let cacert_lifetime = Ptime.Span.v (365, 0L)
  let _10s = Ptime.Span.of_int_s 10
  let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
  let reword_error = Result.map_error

  let make domain_name (_, pk) =
    let ( >>= ) = Result.bind in
    let valid_from = Option.get Ptime.(sub_span (Mirage_ptime.now ()) _10s) in
    Ptime.add_span valid_from cacert_lifetime
    |> Option.to_result ~none:(msgf "End time out of range")
    >>= fun valid_until ->
    X509.Signing_request.create cacert_dn pk >>= fun ca_csr ->
    let extensions =
      let open X509.Extension in
      let key_id =
        X509.Public_key.id X509.Signing_request.((info ca_csr).public_key)
      in
      empty
      |> add Subject_alt_name
           ( true,
             X509.General_name.(
               singleton DNS [ Domain_name.to_string domain_name ]) )
      |> add Basic_constraints (true, (false, None))
      |> add Key_usage
           (true, [ `Digital_signature; `Content_commitment; `Key_encipherment ])
      |> add Subject_key_id (false, key_id)
    in
    X509.Signing_request.sign ~valid_from ~valid_until ~extensions ca_csr pk
      cacert_dn
    |> reword_error (msgf "%a" X509.Validation.pp_signature_error)
    >>= fun certificate -> Ok (certificate, pk)
end

module K = struct
  open Cmdliner
  open Dnsvizor

  let ipv4 =
    Mirage_runtime.register_arg
      (Mirage_runtime_network.V4.network
         (Ipaddr.V4.Prefix.of_string_exn "10.0.0.2/24"))

  let ipv4_gateway =
    Mirage_runtime.register_arg (Mirage_runtime_network.V4.gateway None)

  let ipv4_only =
    Mirage_runtime.register_arg (Mirage_runtime_network.ipv4_only ())

  let ipv6 =
    Mirage_runtime.register_arg (Mirage_runtime_network.V6.network None)

  let ipv6_gateway =
    Mirage_runtime.register_arg (Mirage_runtime_network.V6.gateway None)

  let ipv6_only =
    Mirage_runtime.register_arg (Mirage_runtime_network.ipv6_only ())

  let accept_router_advertisements =
    Mirage_runtime.register_arg
      (Mirage_runtime_network.V6.accept_router_advertisements ())

  let dns_cache =
    let doc = Arg.info ~doc:"DNS cache size" [ "dns-cache" ] in
    Mirage_runtime.register_arg Arg.(value & opt (some int) None doc)

  let dns_upstream =
    let doc =
      Arg.info
        ~doc:
          "Upstream DNS resolver (if specified, a stub resolver is used \
           instead of a recursive)"
        [ "dns-upstream" ]
    in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)

  let dns_blocklist =
    let doc =
      Arg.info ~doc:"A web address to fetch DNS block lists from."
        [ "dns-blocklist-url" ]
    in
    Mirage_runtime.register_arg Arg.(value & opt_all string [] doc)

  let dns_block =
    let doc = Arg.info ~doc:"A domain to block." [ "dns-block" ] in
    let domain = Arg.conv (Domain_name.of_string, Domain_name.pp) in
    Mirage_runtime.register_arg Arg.(value & opt_all domain [] doc)

  (* DNSmasq configuration options *)
  module Dnsmasq = struct
    let s_dnsmasq = "DNSMASQ-COMPATIBLE OPTIONS"

    (* TODO support multiple dhcp-range statements *)
    let dhcp_range =
      let doc =
        Arg.info ~doc:"Enable DHCP server." ~docv:Config_parser.dhcp_range_docv
          ~docs:s_dnsmasq [ "dhcp-range" ]
      in
      Arg.(value & opt (some Config_parser.dhcp_range_c) None doc)

    let dhcp_host =
      let doc =
        Arg.info
          ~doc:
            "A static dhcp-host entry, containing the MAC address, hostname, \
             and IPv4 address."
          ~docv:Config_parser.dhcp_host_docv ~docs:s_dnsmasq [ "dhcp-host" ]
      in
      Arg.(value & opt_all Config_parser.dhcp_host_c [] doc)

    let dhcp_option =
      let doc =
        Arg.info ~doc:"A dhcp option, option code (or name) and value."
          ~docv:Config_parser.dhcp_option_docv ~docs:s_dnsmasq [ "dhcp-option" ]
      in
      Arg.(value & opt_all Config_parser.dhcp_option_c [] doc)

    let domain =
      let doc =
        Arg.info ~doc:"Domain to use." ~docv:Config_parser.domain_docv
          ~docs:s_dnsmasq [ "domain" ]
      in
      Arg.(value & opt (some Config_parser.domain_c) None doc)

    let no_hosts =
      let doc =
        Arg.info
          ~doc:
            "Don't 'read' the (synthesized) /etc/hosts (contains only --name \
             argument)"
          [ "no-hosts" ]
      in
      Arg.(value & flag doc)

    let dnssec =
      let doc =
        Arg.info ~doc:"Validate DNS replies and cache DNSSEC data."
          ~docs:s_dnsmasq [ "dnssec" ]
      in
      Arg.(value & flag doc)

    let bogus_priv =
      let doc =
        Arg.info
          ~doc:
            "Bogus private reverse lookups. Reverse lookups for private\n\
             IP ranges which are not found as DHCP lease are answered with \
             \"no such domain\". See RFC 6303, 6761 and 6762 for the address \
             ranges and domains affected."
          ~docs:s_dnsmasq [ "bogus_priv" ]
      in
      Arg.(value & flag doc)

    (* various ignored DNSmasq configuration options *)
    let interface =
      let doc =
        Arg.info ~docs:Manpage.s_none ~doc:"Interface to listen on."
          [ "interface" ]
      in
      Arg.(value & opt (some (Config_parser.ignore_c "interface")) None doc)

    let except_interface =
      let doc =
        Arg.info ~docs:Manpage.s_none ~doc:"Interface to not listen on."
          [ "except-interface" ]
      in
      Arg.(
        value & opt (some (Config_parser.ignore_c "except-interface")) None doc)

    let listen_address =
      let doc =
        Arg.info ~docs:Manpage.s_none ~doc:"IP address to listen on."
          [ "listen-address" ]
      in
      Arg.(
        value & opt (some (Config_parser.ignore_c "listen-address")) None doc)

    let no_dhcp_interface =
      let doc =
        Arg.info ~docs:Manpage.s_none ~doc:"Only provide DNS service on."
          [ "no-dhcp-interface" ]
      in
      Arg.(
        value & opt (some (Config_parser.ignore_c "no-dhcp-interface")) None doc)

    let bind_interfaces =
      let doc =
        Arg.info ~docs:Manpage.s_none ~doc:"Bind to interface IP address only."
          [ "bind_interfaces" ]
      in
      Arg.(value & flag doc)

    let dhcp_authoritative =
      let doc =
        Arg.info ~docs:Manpage.s_none
          ~doc:
            "Should be set when DNSvizor is definitively the only DHCP server \
             on a network. Assumed by charrua, our DHCP server implementation."
          [ "dhcp-authoritative" ]
      in
      Arg.(value & flag doc)

    let domain_needed =
      let doc =
        Arg.info ~docs:Manpage.s_none
          ~doc:
            "Never forward A or AAAA queries for plain names, without dots or \
             domain parts, to upstream nameservers. If the name is not known \
             from /etc/hosts or DHCP then a \"not found\" answer is returned."
          [ "domain-needed" ]
      in
      Arg.(value & flag doc)
  end

  let dnsmasq : unit -> Config_parser.config =
    Mirage_runtime.register_arg
    @@
    let open Cmdliner.Term.Syntax in
    let open Dnsmasq in
    let ( @? ) x xs = match x with None -> xs | Some x -> x :: xs in
    let+ dhcp_range = dhcp_range
    and+ dhcp_host = dhcp_host
    and+ dhcp_option = dhcp_option
    and+ domain = domain
    and+ no_hosts = no_hosts
    and+ dnssec = dnssec
    and+ bogus_priv = bogus_priv
    and+ _ = interface
    and+ _ = except_interface
    and+ _ = listen_address
    and+ _ = no_dhcp_interface
    and+ _ = bind_interfaces
    and+ _ = dhcp_authoritative
    and+ _ = domain_needed in
    Option.map (fun x -> `Dhcp_range x) dhcp_range
    @? Option.map (fun x -> `Domain x) domain
    @? (if no_hosts then Some `No_hosts else None)
    @? (if dnssec then Some `Dnssec else None)
    @? (if bogus_priv then Some `Bogus_priv else None)
    @? List.map (fun x -> `Dhcp_option x) dhcp_option
    @ List.map (fun x -> `Dhcp_host x) dhcp_host

  let qname_minimisation =
    let doc =
      Arg.info ~doc:"Use qname minimisation (RFC 9156)."
        [ "qname-minimisation" ]
    in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let opportunistic_tls =
    let doc =
      Arg.info
        ~doc:
          "Use opportunistic TLS from recursive resolver to authoriative (RFC \
           9539)."
        [ "opportunistic-tls-authoritative" ]
    in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let https_port =
    let doc = Arg.info ~doc:"The HTTPS port." [ "https-port" ] in
    Mirage_runtime.register_arg Arg.(value & opt int 443 & doc)

  let no_tls =
    let doc =
      Arg.info ~doc:"Disable TLS (HTTPS server and DNS-over-TLS/DNS-over-HTTPS)"
        [ "no-tls" ]
    in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let valid_bits str =
    try
      let bits = int_of_string str in
      bits >= 89
    with _ -> false

  let ca_key_doc =
    "The seed (base64 encoded) used to generate the private key for the \
     certificate. The seed can be prepended by the type of the key (rsa or \
     ed25519) plus a colon. For a RSA key, the user can also specify bits: \
     \"rsa:4096:foo=\"."

  let ca_key =
    let ( let* ) = Result.bind in
    let parser str =
      match String.split_on_char ':' str with
      | "rsa" :: bits :: seed when valid_bits bits ->
          let bits = int_of_string bits in
          let seed = String.concat ":" seed in
          let* seed = Base64.decode seed in
          let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
          Ok (str, `RSA (Mirage_crypto_pk.Rsa.generate ~g ~bits ()))
      | "ed25519" :: seed ->
          let seed = String.concat ":" seed in
          let* seed = Base64.decode seed in
          let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
          let pk, _ = Mirage_crypto_ec.Ed25519.generate ~g () in
          Ok (str, `ED25519 pk)
      | "rsa" :: seed | seed ->
          let seed = String.concat ":" seed in
          let* seed = Base64.decode seed in
          let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
          Ok (str, `RSA (Mirage_crypto_pk.Rsa.generate ~g ~bits:4096 ()))
    in
    let pp ppf (str, _) = Fmt.string ppf str in
    Mirage_runtime.register_arg
      Arg.(
        value
        & opt (some (conv (parser, pp))) None
        & info [ "ca-seed" ] ~doc:ca_key_doc)

  let password =
    let doc = Arg.info ~doc:"Password used for authentication" [ "password" ] in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)

  let hostname =
    let ( let* ) = Result.bind in
    let parser str =
      let* dn = Domain_name.of_string str in
      Domain_name.host dn
    in
    let pp = Domain_name.pp in
    let domain_name = Arg.conv (parser, pp) in
    let doc =
      "The hostname (SNI for the certificate, entry in DNS) of the unikernel."
    in
    Mirage_runtime.register_arg
      Arg.(
        required
        & opt (some domain_name)
            (Some Domain_name.(of_string_exn "dnsvizor" |> host_exn))
        & info [ "hostname" ] ~doc)

  let key_v =
    Arg.conv ~docv:"HOST:HASH:DATA"
      Dns.Dnskey.
        (name_key_of_string, fun ppf v -> Fmt.string ppf (name_key_to_string v))

  let dns_key =
    let doc =
      "The nsupdate key to use for updating the name server whenever a client \
       requests this (via DHCP FQDN, RFC 4702)"
    in
    Mirage_runtime.register_arg
      Arg.(value & opt (some key_v) None & info ~doc [ "dns-key" ])

  let dns_server =
    let doc =
      "The IP address of the DNS server to update with new host names"
    in
    Mirage_runtime.register_arg
      Arg.(
        value
        & opt (some Mirage_runtime_network.Arg.ip_address) None
        & info ~doc [ "dns-server" ])

  let tlstunnel_key =
    let doc =
      "The shared secret of the TLSTUNNEL server to update with new host names"
    in
    Mirage_runtime.register_arg
      Arg.(value & opt (some string) None & info ~doc [ "tlstunnel-key" ])

  let tlstunnel_server =
    let doc = "The IP address of the TLSTUNNEL server" in
    Mirage_runtime.register_arg
      Arg.(
        value
        & opt (some Mirage_runtime_network.Arg.ip_address) None
        & info ~doc [ "tlstunnel-server" ])
end

module Net (N : Mirage_net.S) = struct
  (* A Mirage_net.S implementation which diverts DHCP messages to a DHCP
     server. The DHCP server needs to get the entire Ethernet frame, because
     the Ethernet source address is the address to send replies to, its IPv4
     addresses (source, destination) do not matter (since the DHCP client that
     sent this request does not have an IP address yet). ARP cannot be used
     by DHCP, because the client does not have an IP address (and thus no ARP
     replies). *)

  type error = N.error

  let pp_error = N.pp_error

  type nonrec t = {
    net : N.t;
    mutable config : Dhcp_server.Config.t option;
    mutable leases : Dhcp_server.Lease.database;
    mutable lease_acquired :
      Dhcp_server.Lease.t ->
      Dhcp_wire.dhcp_option list ->
      (Dhcp_wire.dhcp_option list, unit) result Lwt.t;
  }

  let write t = N.write t.net

  let handle_dhcp t config buf =
    match Dhcp_wire.pkt_of_buf buf (Cstruct.length buf) with
    | Error `Not_dhcp -> Lwt.return_unit
    | Error (`Msg e) ->
        Logs.err (fun m -> m "Can't parse packet: %s" e);
        Lwt.return_unit
    | Ok pkt -> (
        let now =
          Mirage_mtime.elapsed_ns () |> Duration.to_sec |> Int32.of_int
        in
        match Dhcp_server.Input.input_pkt config t.leases pkt now with
        | Dhcp_server.Input.Silence -> Lwt.return_unit
        | Dhcp_server.Input.Update (_lease_opt, leases) ->
            (* if lease_opt is present, the lease got removed! *)
            t.leases <- leases;
            Logs.debug (fun m ->
                m "Received packet %a - updated lease database" Dhcp_wire.pp_pkt
                  pkt);
            Lwt.return_unit
        | Dhcp_server.Input.Warning w ->
            Logs.warn (fun m -> m "%s" w);
            Lwt.return_unit
        | Dhcp_server.Input.Error e ->
            Logs.err (fun m -> m "%s" e);
            Lwt.return_unit
        | Dhcp_server.Input.Reply (reply, lease_opt, leases) -> (
            (match lease_opt with
              | None -> Lwt.return (Ok reply)
              | Some (lease, opts) -> (
                  Logs.info (fun m ->
                      m "Handing out lease %s, received options %a"
                        (Dhcp_server.Lease.to_string lease)
                        Fmt.(list ~sep:(any ", ") string)
                        (List.map Dhcp_wire.dhcp_option_to_string opts));
                  t.lease_acquired lease opts >>= function
                  | Ok options ->
                      let reply =
                        Dhcp_wire.
                          { reply with options = reply.options @ options }
                      in
                      Lwt.return (Ok reply)
                  | Error _ as e -> Lwt.return e))
            >>= function
            | Error () -> Lwt.return_unit
            | Ok reply -> (
                Logs.info (fun m -> m "Sending reply %a" Dhcp_wire.pp_pkt reply);
                t.leases <- leases;
                N.write t.net
                  ~size:(N.mtu t.net + Ethernet.Packet.sizeof_ethernet)
                  (Dhcp_wire.pkt_into_buf reply)
                >|= function
                | Ok () -> ()
                | Error ne ->
                    Logs.err (fun m ->
                        m "got a network error: %a" N.pp_error ne))))

  let listen t ~header_size net =
    let dhcp_or_not buf =
      let of_interest hdr =
        let dst = hdr.Ethernet.Packet.destination in
        Macaddr.compare dst (N.mac t.net) = 0 || not (Macaddr.is_unicast dst)
      in
      match t.config with
      | None -> net buf
      | Some config -> (
          match Ethernet.Packet.of_cstruct buf with
          | Ok (eth_header, _)
            when of_interest eth_header
                 && Dhcp_wire.is_dhcp buf (Cstruct.length buf) ->
              handle_dhcp t config buf
          | _ -> net buf)
    in
    N.listen t.net ~header_size dhcp_or_not

  let connect net config =
    let leases = Dhcp_server.Lease.make_db () in
    let lease_acquired _ _ = Lwt.return (Ok []) in
    { net; config; leases; lease_acquired }

  let disconnect _ =
    Logs.warn (fun m -> m "ignoring disconnect");
    Lwt.return_unit

  let mac t = N.mac t.net
  let mtu t = N.mtu t.net
  let get_stats_counters t = N.get_stats_counters t.net
  let reset_stats_counters t = N.reset_stats_counters t.net
end

module Main (N : Mirage_net.S) (ASSETS : Mirage_kv.RO) = struct
  module Net = Net (N)
  module ETH = Ethernet.Make (Net)
  module ARP = Arp.Make (ETH)
  module IPV4 = Static_ipv4.Make (ETH) (ARP)
  module IPV6 = Ipv6.Make (Net) (ETH)
  module IPV4V6 = Tcpip_stack_direct.IPV4V6 (IPV4) (IPV6)
  module ICMP = Icmpv4.Make (IPV4)
  module UDP = Udp.Make (IPV4V6)
  module TCP = Tcp.Flow.Make (IPV4V6)

  module S =
    Tcpip_stack_direct.MakeV4V6 (Net) (ETH) (ARP) (IPV4V6) (ICMP) (UDP) (TCP)

  module Resolver = Dns_resolver_mirage.Make (S)
  module Stub = Dns_stub_mirage.Make (S)
  module HTTP = Paf_mirage.Make (TCP)
  module HE = Happy_eyeballs_mirage.Make (S)
  module Dns_client = Dns_client_mirage.Make (S) (HE)
  module Mimic_he = Mimic_happy_eyeballs.Make (S) (HE) (Dns_client)
  module Http_client = Http_mirage_client.Make (TCP) (Mimic_he)
  module Map = Map.Make (String)

  type t = { net : Net.t; mac : Macaddr.t; mutable configuration : string }

  type intermediate_config = {
    dhcp_hosts :
      (string list * Macaddr.t list * string * Ipaddr.V4.t option) list;
    dhcp_range : (Ipaddr.V4.t * Ipaddr.V4.t option * int option) option;
    dhcp_options : Dhcp_wire.dhcp_option list;
    tagged_dhcp_options : Dhcp_wire.dhcp_option list Map.t;
    domain : [ `raw ] Domain_name.t option;
    no_hosts : bool;
    dnssec : bool;
    bogus_priv : bool;
  }

  let process_config configuration =
    let ( let* ) = Result.bind in
    let unique x item =
      match x with
      | None -> Ok ()
      | Some _ -> Error (item ^ " must only appear once")
    in
    let rec gather acc : Dnsvizor.Config_parser.config -> _ = function
      | [] -> Ok acc
      | `Ignored :: r -> gather acc r
      | `Dhcp_range
          ( { mode = Some _; _ }
          | { broadcast = Some _; _ }
          | { netmask = Some _; _ } )
        :: _ ->
          Error "Unhandled dhcp-range option"
      | `Dhcp_range dhcp_range :: r ->
          (* TODO: multiple ranges *)
          (* TODO: netmask *)
          let* () = unique acc.dhcp_range "dhcp-range" in
          let range =
            (dhcp_range.start_addr, dhcp_range.end_addr, dhcp_range.lease_time)
          in
          gather { acc with dhcp_range = Some range } r
      | `Dhcp_option { vendor = Some _; _ } :: _ ->
          Error "Don't know how to handle vendor in --dhcp-option (yet)"
      | `Dhcp_option
          {
            option =
              ( Dhcp_wire.Log_servers _ | Dhcp_wire.Vendor_specific _
              | Dhcp_wire.Routers _ | Dhcp_wire.Dns_servers _ ) as option;
            tags = [];
          }
        :: r ->
          gather { acc with dhcp_options = option :: acc.dhcp_options } r
      | `Dhcp_option
          {
            option =
              ( Dhcp_wire.Log_servers _ | Dhcp_wire.Vendor_specific _
              | Dhcp_wire.Routers _ | Dhcp_wire.Dns_servers _ ) as option;
            tags = [ tag ];
          }
        :: r ->
          (* TODO: if there are multiple tags then *all* tags must be matched.
             So to make it simpler we only consider one tag for now. *)
          let tagged_dhcp_options =
            (* OCaml>=5.1: use Map.add_to_list *)
            Map.update tag
              (function
                | None -> Some [ option ]
                | Some options -> Some (option :: options))
              acc.tagged_dhcp_options
          in
          gather { acc with tagged_dhcp_options } r
      | `Dhcp_option ({ tags = _a :: _b :: _ } as dhcp_option) :: _ ->
          Error
            (Fmt.str
               "Don't know how to handle dhcp-option with multiple tags %a"
               Dnsvizor.Config_parser.pp_dhcp_option dhcp_option)
      | `Dhcp_option ({ option = _ } as dhcp_option) :: _ ->
          Error
            (Fmt.str "Don't know how to handle dhcp-option %a"
               Dnsvizor.Config_parser.pp_dhcp_option dhcp_option)
      | `Dhcp_host
          {
            id = None;
            tags = [];
            ipv6 = None;
            lease_time = None;
            ignore = false;
            sets;
            domain_name = Some domain_name;
            macs;
            ipv4;
          }
        :: r ->
          gather
            {
              acc with
              dhcp_hosts =
                (sets, macs, Domain_name.to_string domain_name, ipv4)
                :: acc.dhcp_hosts;
            }
            r
      | `Dhcp_host h :: _ ->
          Error
            (Fmt.str "Don't know how to handle dhcp-host with these options: %a"
               Dnsvizor.Config_parser.pp_dhcp_host h)
      | `Domain (_, Some _) :: _ ->
          Error
            "Don't know how to handle domain with address range or interface"
      | `Domain (domain, None) :: r ->
          let* () = unique acc.domain "domain" in
          gather { acc with domain = Some domain } r
      | `No_hosts :: r -> gather { acc with no_hosts = true } r
      | `Dnssec :: r -> gather { acc with dnssec = true } r
      | `Bogus_priv :: r -> gather { acc with bogus_priv = true } r
    in
    gather
      {
        dhcp_hosts = [];
        dhcp_range = None;
        dhcp_options = [];
        tagged_dhcp_options = Map.empty;
        domain = None;
        no_hosts = false;
        dnssec = false;
        bogus_priv = false;
      }
      configuration

  let dhcp_configuration_of mac config =
    let ( let* ) = Result.bind in
    let ipv4 = K.ipv4 () in
    let ipv4_address = Ipaddr.V4.Prefix.address ipv4 in
    let* {
           dhcp_hosts;
           dhcp_range;
           dhcp_options;
           tagged_dhcp_options;
           domain;
           no_hosts;
           dnssec;
           bogus_priv;
         } =
      process_config config
    in
    let* range, default_lease_time =
      match dhcp_range with
      | None -> Ok (None, None)
      | Some (start, None, lease_time) ->
          if Ipaddr.V4.Prefix.mem start ipv4 then
            Ok (Some (start, Ipaddr.V4.Prefix.last ipv4), lease_time)
          else
            Error
              ("Don't know what to do with dhcp-range "
             ^ Ipaddr.V4.to_string start)
      | Some (start, Some stop, lease_time) ->
          if Ipaddr.V4.Prefix.mem start ipv4 && Ipaddr.V4.Prefix.mem stop ipv4
          then Ok (Some (start, stop), lease_time)
          else
            Error
              (Fmt.str "Don't know what to do with dhcp-range %a,%a"
                 Ipaddr.V4.pp start Ipaddr.V4.pp stop)
    in
    let router =
      Option.to_list
        (Option.map (fun x -> Dhcp_wire.Routers [ x ]) (K.ipv4_gateway ()))
    and dns_server = [ Dhcp_wire.Dns_servers [ ipv4_address ] ] in
    let options =
      (if
         List.exists
           (function Dhcp_wire.Routers _ -> true | _ -> false)
           dhcp_options
       then []
       else router)
      @ (if
           List.exists
             (function Dhcp_wire.Dns_servers _ -> true | _ -> false)
             dhcp_options
         then []
         else dns_server)
      @ dhcp_options
      @ Option.to_list
          (Option.map
             (fun domain ->
               Dhcp_wire.Domain_name (Domain_name.to_string domain))
             domain)
    in
    let hosts =
      List.concat_map
        (fun (sets, macs, hostname, fixed_addr) ->
          List.map
            (fun hw_addr ->
              let options =
                List.fold_left
                  (fun acc tag ->
                    match Map.find_opt tag tagged_dhcp_options with
                    | None -> acc
                    | Some additional_options ->
                        (* XXX: do we need to deduplicate?! *)
                        additional_options @ acc)
                  options sets
              in
              { Dhcp_server.Config.hw_addr; hostname; fixed_addr; options })
            macs)
        dhcp_hosts
    in
    let max_lease_time = Option.map (fun lt -> lt * 2) default_lease_time in
    let dhcp_config =
      Dhcp_server.Config.make ?hostname:None ?default_lease_time ?max_lease_time
        ~hosts ~addr_tuple:(ipv4_address, mac)
        ~network:(Ipaddr.V4.Prefix.prefix ipv4)
        ~range ~options ()
    in
    Ok (dhcp_config, domain, no_hosts, dnssec, bogus_priv)

  let of_commandline mac () =
    let ( let+ ) x f = Result.map f x in
    let configuration = K.dnsmasq () in
    let+ dhcp_configuration, domain, no_hosts, dnssec, bogus_priv =
      dhcp_configuration_of mac configuration
    in
    let config =
      Fmt.to_to_string (Dnsvizor.Config_parser.pp_config `File) configuration
    in
    (config, dhcp_configuration, domain, no_hosts, dnssec, bogus_priv)

  let update_configuration t config
      (parsed_config : Dnsvizor.Config_parser.config) =
    let ( let+ ) x f = Result.map f x in
    let+ dhcp_configuration, domain, no_hosts, dnssec, bogus_priv =
      Result.map_error (fun s -> `Msg s)
      @@ dhcp_configuration_of t.mac parsed_config
    in
    t.configuration <- config;
    t.net.config <- Some dhcp_configuration;
    (domain, no_hosts, dnssec, bogus_priv)

  let lookup_src_by_name name =
    List.find_opt (fun src -> Metrics.Src.name src = name) (Metrics.Src.list ())

  module Daemon (Resolver : Dns_resolver_mirage_shared.S) = struct
    let pp_error ppf = function
      | `Bad_gateway -> Fmt.string ppf "Bad gateway"
      | `Bad_request -> Fmt.string ppf "Bad request"
      | `Exn exn -> Fmt.pf ppf "Exception: %s" (Printexc.to_string exn)
      | `Internal_server_error -> Fmt.string ppf "Internal server error"

    let error : type reqd headers request response ro wo.
        Ipaddr.t * int ->
        (reqd, headers, request, response, ro, wo) Alpn.protocol ->
        ?request:request ->
        Alpn.server_error ->
        (headers -> wo) ->
        unit =
     fun (ipaddr, port) protocol ?request:_ err _writer ->
      match protocol with
      | Alpn.HTTP_1_1 _ | Alpn.H2 _ ->
          Logs.err (fun m ->
              m "Got an error from %a:%d: %a" Ipaddr.pp ipaddr port pp_error err)

    let to_map ~assoc m =
      let open Multipart_form in
      let rec go (map, rest) = function
        | Leaf { header; body } -> (
            match
              Option.bind
                (Header.content_disposition header)
                Content_disposition.name
            with
            | Some name -> (
                match List.assoc_opt body assoc with
                | Some value -> (Map.add name (None, value) map, rest)
                | None -> (map, rest))
            | None ->
                Logs.warn (fun m ->
                    m "multipart-form: discarding field with no name.");
                (map, rest))
        | Multipart { body; _ } ->
            List.fold_left
              (fun acc elt ->
                match elt with Some elt -> go acc elt | None -> acc)
              (map, rest) body
      in
      go (Map.empty, []) m

    let authenticate_user ~auth_password ~system_password content =
      match (system_password, auth_password) with
      | Some system_password, Some http_password ->
          if String.equal http_password system_password then content
          else (
            Logs.warn (fun m ->
                m
                  "password-auth: Passwords do not match, authentication \
                   failed.");
            Some (`Authenticate ("Passwords do not match", None)))
      | None, _ ->
          (* No password provided at startup *)
          Logs.err (fun m -> m "password-auth: No password provided at startup");
          Some (`Authenticate ("Unikernel wasn't given a password", None))
      | _, None ->
          (* No password provided in the request *)
          Logs.err (fun m -> m "password-auth: No password in http request");
          Some (`Authenticate ("User didn't provide a password", None))

    let query_info_to_json_str qi =
      let Dns_resolver_mirage_shared.
            { fin; question; src; rcode; time_taken; status } =
        qi
      in
      let qtyp =
        match Dns.Packet.Question.qtype question with
        | Some qtyp -> Fmt.to_to_string Dns.Packet.Question.pp_qtype qtyp
        | None -> "NONE"
      in
      Format.sprintf
        {|{ "fin": %S, "typ": %S, "domain": %S, "client": %S, "rcode": %S, "time_taken": %u, "status": %S }|}
        (Ptime.to_rfc3339 ~tz_offset_s:0 fin)
        qtyp
        (Domain_name.to_string (fst question))
        (Ipaddr.to_string src)
        (Dns.Rcode.to_string rcode)
        (Duration.to_ms time_taken)
        status

    let web_ui_handler t resolver query_buffer js_file system_password
        req_method path auth_password =
      let get_multipart_body content_type_header data field =
        let content_type =
          Option.fold ~none:"application/x-www-form-urlencoded\r\n"
            ~some:(fun s -> s ^ "\r\n")
            content_type_header
        in
        match Multipart_form.Content_type.of_string content_type with
        | Error (`Msg e) ->
            Logs.err (fun m -> m "Bad content-type header: %s" e);
            Error (`Msg "Bad content-type header")
        | Ok ct -> (
            match Multipart_form.of_string_to_list data ct with
            | Error (`Msg e) ->
                Logs.err (fun m -> m "Error parsing form: %s" e);
                Error (`Msg "Error parsing form")
            | Ok (m, assoc) -> (
                let multipart_body, _r = to_map ~assoc m in
                match Map.find_opt field multipart_body with
                | None ->
                    Logs.err (fun m -> m "No %s in form" field);
                    Error (`Msg (field ^ " not found in form"))
                | Some (_, body_str) -> Ok body_str))
      in
      match (req_method, path) with
      | `GET, "/main.js" -> Some (`Content (js_file, Some "text/javascript"))
      | `GET, "/" | `GET, "/dashboard" ->
          let map = Metrics.get_cache () in
          let lookup_stats name =
            match lookup_src_by_name name with
            | None -> []
            | Some src -> (
                (* TODO handle multiple measurements with different tags? *)
                match Metrics.SM.find_opt src map with
                | None -> []
                | Some datas ->
                    List.concat_map
                      (fun (_tag, data) -> Metrics.Data.fields data)
                      datas)
          in
          let find_measurement ?(default = -2) fields field_name =
            let field =
              List.find_opt (fun field -> Metrics.key field = field_name) fields
            in
            let value = Option.map Metrics.value field in
            match value with
            | Some (Metrics.V (Metrics.Uint, i)) -> (i :> int)
            | _ -> default
          in
          let resolv_stats =
            let fields = lookup_stats "dns-resolver" in
            let clients = find_measurement ~default:0 fields "clients"
            and queries = find_measurement ~default:0 fields "queries"
            and blocked_requests = find_measurement ~default:0 fields "blocked"
            and errors = find_measurement ~default:0 fields "error" in
            (clients, queries, blocked_requests, errors)
          in
          let dns_cache_stats =
            let fields = lookup_stats "dns-cache" in
            let weight = find_measurement fields "weight"
            and capacity = find_measurement fields "capacity" in
            (weight, capacity)
          in
          let resolver_timing =
            let fields = lookup_stats "dns-resolver-timings" in
            find_measurement ~default:0 fields "mean response"
          in
          let memory_stats =
            let fields = lookup_stats "memory" in
            let live = find_measurement fields "memory live words"
            and free = find_measurement fields "memory free words" in
            (live, free)
          in
          let gc_stats =
            let fields = lookup_stats "gc" in
            let live = find_measurement fields Metrics.Key.live_words
            and free = find_measurement fields Metrics.Key.free_words in
            (live, free)
          in
          let domains_on_blocklist =
            Blocklist.number_of_blocked_domains (Resolver.primary_data resolver)
          in
          let content =
            Statistics.statistics_page resolv_stats dns_cache_stats
              resolver_timing memory_stats gc_stats domains_on_blocklist
          in
          Some (`Content (Dashboard.dashboard_layout ~content (), None))
      | `GET, "/querylog" ->
          Some
            (`Content
               ( Dashboard.dashboard_layout ~content:Query_logs.query_page (),
                 None ))
      | `GET, "/api/queries" ->
          let response closed write_and_flush data =
            if closed () then Error ()
            else (
              write_and_flush ("data:" ^ data ^ "\n\n");
              Ok ())
          in
          let f () =
            Lwt_condition.wait (Resolver.queries resolver) >|= fun query ->
            query_info_to_json_str query
          and buffered = List.map query_info_to_json_str !query_buffer in
          Some (`Stream (response, f, buffered))
      | `GET, "/blocklist" ->
          let content =
            Blocklist.block_page
              (Blocklist.blocked_domains (Resolver.primary_data resolver))
          in
          authenticate_user ~auth_password ~system_password
            (Some (`Content (Dashboard.dashboard_layout ~content (), None)))
      | `GET, "/configuration" ->
          authenticate_user ~auth_password ~system_password
            (Some
               (`Content
                  ( Dashboard.dashboard_layout
                      ~content:
                        (Configuration.configuration_page t.configuration)
                      (),
                    None )))
      | `POST (content_type_header, data), "/blocklist/add" ->
          authenticate_user ~auth_password ~system_password
            (match get_multipart_body content_type_header data "domain" with
            | Ok domain -> (
                match Domain_name.of_string domain with
                | Error (`Msg e) ->
                    Logs.debug (fun m ->
                        m "client wanted to add a bad domain: %s" e);
                    Some (`Bad_request ("/blocklist", None))
                | Ok domain ->
                    let trie = Resolver.primary_data resolver in
                    let trie =
                      Dns_trie.insert domain Dns.Rr_map.Soa
                        (Blocklist.soa "web-ui" 0l)
                        trie
                    in
                    Resolver.update_primary_data resolver trie;
                    Some (`Redirect ("/blocklist", None)))
            | Error (`Msg e) ->
                Logs.err (fun m -> m "multipart-form error: %s" e);
                Some (`Bad_request ("/blocklist", None)))
      | `POST (content_type_header, data), "/configuration/upload" ->
          authenticate_user ~auth_password ~system_password
            (match
               get_multipart_body content_type_header data "dnsmasq_config"
             with
            | Ok config_data -> (
                match
                  Result.bind
                    (Dnsvizor.Config_parser.parse_file config_data)
                    (update_configuration t config_data)
                with
                | Ok (_domain, _no_hosts, _dnssec, _bogus_priv) ->
                    (* TODO: handle domain, no_hosts, dnssec, bogus_priv *)
                    Logs.info (fun m -> m "Dnsmasq config parsed correctly");
                    Some (`Redirect ("/configuration", None))
                | Error (`Msg err) ->
                    Logs.err (fun m ->
                        m "Error parsing dnsmasq configuration: %s" err);
                    Some (`Bad_request ("/configuration", None)))
            | Error (`Msg e) ->
                Logs.err (fun m -> m "multipart-form error: %s" e);
                Some (`Bad_request ("/configuration", None)))
      | `POST _, s when String.starts_with s ~prefix:"/blocklist/delete/" ->
          authenticate_user ~auth_password ~system_password
            ((* NOTE: here we don't need the body because we embed in the path *)
             let off = String.length "/blocklist/delete/" in
             let domain = String.sub s off (String.length s - off) in
             match Domain_name.of_string domain with
             | Error _ -> None
             | Ok domain ->
                 let trie = Resolver.primary_data resolver in
                 let trie = Dns_trie.remove_all domain trie in
                 Resolver.update_primary_data resolver trie;
                 Some (`Redirect ("/blocklist", None)))
      | `POST _, "/blocklist/update" ->
          Some (`Redirect ("/blocklist", Some `Update))
      | _ -> None

    let authenticate_header =
      ( "www-authenticate",
        "Basic realm=\"DNSvizor wants you to authenticate to access this (use \
         any username)\", charset=\"UTF-8\"" )

    let extract_password = function
      | Some header when String.length header >= 5 -> (
          match
            Base64.decode
              (String.trim (String.sub header 5 (String.length header - 5)))
            (*remove basic prefix and decode*)
          with
          | Ok decoded -> (
              let parts = String.split_on_char ':' decoded in
              match parts with
              | [ password ] -> Some password
              | _username :: password -> Some (String.concat ":" password)
              | _ ->
                  Logs.warn (fun m ->
                      m "password-auth: Bad authorization header");
                  None)
          | Error _ ->
              Logs.warn (fun m ->
                  m "password-auth: Bad base64 encoding in header");
              None)
      | Some _ ->
          Logs.warn (fun m -> m "password-auth: Authorization header too short");
          None
      | None -> None

    let security_headers =
      [
        ("x-frame-options", "DENY");
        ("content-security-policy", "frame-ancestors 'none'");
      ]

    let request : type reqd headers request response ro wo.
        _ ->
        _ ->
        _ ->
        _ ->
        HTTP.TLS.flow ->
        Ipaddr.t * int ->
        reqd ->
        (reqd, headers, request, response, ro, wo) Alpn.protocol ->
        string ->
        string option ->
        unit =
     fun t resolver query_buffer mvar _flow (dst, port) reqd protocol js_file
         password ->
      match protocol with
      | Alpn.HTTP_1_1 (module Reqd) ->
          Lwt.async (fun () ->
              let reply ?(content_type = "text/html") reqd ?(headers = [])
                  ?(status = `OK) data =
                let headers =
                  H1.Headers.of_list
                    (security_headers
                    @ [
                        ("content-type", content_type);
                        ("content-length", string_of_int (String.length data));
                      ]
                    @ headers)
                in
                let resp = H1.Response.create ~headers status in
                Reqd.respond_with_string reqd resp data;
                Lwt.return_unit
              in
              Logs.debug (fun m -> m "Got a new HTTPS request!");
              let request = Reqd.request reqd in
              let auth_password =
                extract_password
                  (H1.Headers.get request.headers "Authorization")
              in
              Logs.info (fun m ->
                  m "HTTPS %a %s" H1.Method.pp_hum request.H1.Request.meth
                    request.H1.Request.target);
              let r =
                match request.H1.Request.meth with
                | `GET -> Lwt.return (Ok `GET)
                | `POST ->
                    (* TODO: reasonable max size *)
                    let initial_size =
                      Option.bind
                        (H1.Headers.get request.headers "content-length")
                        int_of_string_opt
                      |> Option.value ~default:65536
                    in
                    let response_body = H1.Reqd.request_body reqd in
                    let finished, notify_finished = Lwt.wait () in
                    let wakeup v = Lwt.wakeup_later notify_finished v in
                    let on_eof data () = wakeup (Buffer.contents data) in
                    let rec on_read on_eof acc bs ~off ~len =
                      let str = Bigstringaf.substring ~off ~len bs in
                      let () = Buffer.add_string acc str in
                      H1.Body.Reader.schedule_read response_body
                        ~on_read:(on_read on_eof acc) ~on_eof:(on_eof acc)
                    in
                    let f_init = Buffer.create initial_size in
                    H1.Body.Reader.schedule_read response_body
                      ~on_read:(on_read on_eof f_init) ~on_eof:(on_eof f_init);
                    let content_type =
                      H1.Headers.get request.headers "content-type"
                    in
                    finished >|= fun data -> Ok (`Post (content_type, data))
                | _ -> Lwt.return (Error `Method_not_allowed)
              in
              r
              >|= Result.map (fun meth ->
                  ( web_ui_handler t resolver query_buffer js_file password meth
                      request.H1.Request.target auth_password,
                    meth ))
              >>= function
              | Ok (Some (`Content (content, content_type)), _) ->
                  reply ?content_type reqd content
              | Ok (Some (`Authenticate (content, content_type)), _) ->
                  let headers = [ authenticate_header ] in
                  reply ?content_type reqd ~headers ~status:`Unauthorized
                    content
              | Ok (Some (`Redirect (location, action)), _) -> (
                  let headers =
                    H1.Headers.of_list
                      (("location", location) :: security_headers)
                  in
                  let resp = H1.Response.create ~headers `See_other in
                  Reqd.respond_with_string reqd resp "";
                  match action with
                  | None -> Lwt.return_unit
                  | Some `Update ->
                      if Lwt_mvar.is_empty mvar then Lwt_mvar.put mvar `Update
                      else Lwt.return_unit)
              | Ok (Some (`Bad_request (location, action)), _) -> (
                  let headers =
                    H1.Headers.of_list
                      (("location", location) :: security_headers)
                  in
                  let resp = H1.Response.create ~headers `Bad_request in
                  Reqd.respond_with_string reqd resp "";
                  match action with
                  | None -> Lwt.return_unit
                  | Some `Update ->
                      if Lwt_mvar.is_empty mvar then Lwt_mvar.put mvar `Update
                      else Lwt.return_unit)
              | Ok (Some (`Stream (r, f, buffered)), _) ->
                  let headers =
                    H1.Headers.of_list
                      (("Content-Type", "text/event-stream") :: security_headers)
                  in
                  let response = H1.Response.create ~headers `OK in
                  let writer = H1.Reqd.respond_with_streaming reqd response in
                  let reply =
                    r
                      (fun () -> H1.Body.Writer.is_closed writer)
                      (fun str ->
                        H1.Body.Writer.write_string writer str;
                        H1.Body.Writer.flush writer Fun.id)
                  in
                  List.iter (fun d -> reply d |> ignore) buffered;
                  let rec loop () =
                    f () >>= fun data ->
                    match reply data with
                    | Error () -> Lwt.return_unit
                    | Ok () -> loop ()
                  in
                  loop ()
              | Ok (None, _meth) ->
                  let headers =
                    H1.Headers.of_list
                      (("connection", "close") :: security_headers)
                  in
                  let resp = H1.Response.create ~headers `Not_found in
                  Reqd.respond_with_string reqd resp "";
                  Lwt.return_unit
              (* HTTP/2 (RFC7540) is the minimum RECOMMENDED version of HTTP for use
                 with DoH. https://datatracker.ietf.org/doc/html/rfc8484#section-5.2 *)
              | Error status ->
                  let headers =
                    H1.Headers.of_list
                      (("connection", "close") :: security_headers)
                  in
                  let resp = H1.Response.create ~headers status in
                  Reqd.respond_with_string reqd resp "";
                  Lwt.return_unit)
      | Alpn.H2 (module Reqd) ->
          let reply ?(content_type = "text/html") reqd ?(headers = [])
              ?(status = `OK) data =
            let headers =
              H2.Headers.of_list
                (security_headers
                @ [
                    ("content-type", content_type);
                    ("content-length", string_of_int (String.length data));
                  ]
                @ headers)
            in
            let resp = H2.Response.create ~headers status in
            Reqd.respond_with_string reqd resp data
          in
          Logs.debug (fun m -> m "Got a new HTTPS2 request!");
          let request = Reqd.request reqd in
          let auth_password =
            extract_password (H2.Headers.get request.headers "authorization")
          in
          Logs.info (fun m ->
              m "HTTPS2 %a %s" H2.Method.pp_hum request.H2.Request.meth
                request.H2.Request.target);
          let r =
            match request.H2.Request.meth with
            | `GET -> Lwt.return (Ok `GET)
            | `POST ->
                (* TODO: reasonable max size *)
                let initial_size =
                  Option.bind
                    (H2.Headers.get request.headers "content-length")
                    int_of_string_opt
                  |> Option.value ~default:65536
                in
                let response_body = H2.Reqd.request_body reqd in
                let finished, notify_finished = Lwt.wait () in
                let wakeup v = Lwt.wakeup_later notify_finished v in
                let on_eof data () = wakeup (Buffer.contents data) in
                let rec on_read on_eof acc bs ~off ~len =
                  let str = Bigstringaf.substring ~off ~len bs in
                  let () = Buffer.add_string acc str in
                  H2.Body.Reader.schedule_read response_body
                    ~on_read:(on_read on_eof acc) ~on_eof:(on_eof acc)
                in
                let f_init = Buffer.create initial_size in
                H2.Body.Reader.schedule_read response_body
                  ~on_read:(on_read on_eof f_init) ~on_eof:(on_eof f_init);
                let content_type =
                  H2.Headers.get request.headers "content-type"
                in
                finished >|= fun data -> Ok (`POST (content_type, data))
            | _ -> Lwt.return (Error `Method_not_allowed)
          in
          Lwt.async (fun () ->
              r
              >|= Result.map (fun meth ->
                  ( web_ui_handler t resolver query_buffer js_file password meth
                      request.H2.Request.target auth_password,
                    meth ))
              >|= function
              | Ok (Some (`Content (content, content_type)), _) ->
                  reply ?content_type reqd content
              | Ok (Some (`Authenticate (content, content_type)), _) ->
                  let headers = [ authenticate_header ] in
                  reply ?content_type reqd ~headers ~status:`Unauthorized
                    content
              | Ok (Some (`Redirect (location, action)), _) -> (
                  let headers =
                    H2.Headers.of_list
                      (("location", location) :: security_headers)
                  in
                  let resp = H2.Response.create ~headers `See_other in
                  Reqd.respond_with_string reqd resp "";
                  match action with
                  | None -> ()
                  | Some `Update ->
                      Lwt.async (fun () ->
                          if Lwt_mvar.is_empty mvar then
                            Lwt_mvar.put mvar `Update
                          else Lwt.return_unit))
              | Ok (Some (`Bad_request (location, action)), _) -> (
                  let headers =
                    H2.Headers.of_list
                      (("location", location) :: security_headers)
                  in
                  let resp = H2.Response.create ~headers `Bad_request in
                  Reqd.respond_with_string reqd resp "";
                  match action with
                  | None -> ()
                  | Some `Update ->
                      Lwt.async (fun () ->
                          if Lwt_mvar.is_empty mvar then
                            Lwt_mvar.put mvar `Update
                          else Lwt.return_unit))
              | Ok (Some (`Stream (r, f, buffered)), _) ->
                  let headers =
                    H2.Headers.of_list
                      (("content-type", "text/event-stream") :: security_headers)
                  in
                  let response = H2.Response.create ~headers `OK in
                  let writer = H2.Reqd.respond_with_streaming reqd response in
                  let reply =
                    r
                      (fun () -> H2.Body.Writer.is_closed writer)
                      (fun str ->
                        H2.Body.Writer.write_string writer str;
                        H2.Body.Writer.flush writer (fun _ -> ()))
                  in
                  List.iter (fun d -> reply d |> ignore) buffered;
                  let rec loop () =
                    f () >>= fun data ->
                    match reply data with
                    | Error () -> Lwt.return_unit
                    | Ok () -> loop ()
                  in
                  Lwt.async loop
              | Ok (None, meth) -> (
                  match (meth, request.H2.Request.target) with
                  | `GET, path when String.starts_with ~prefix:"/dns-query" path
                    -> (
                      let target = request.H2.Request.target in
                      let elts = String.split_on_char '=' target in
                      let elts = List.tl elts in
                      let query = String.concat "=" elts in
                      Logs.debug (fun m -> m "%s" query);
                      match
                        Base64.decode ~alphabet:Base64.uri_safe_alphabet
                          ~pad:false query
                      with
                      | Ok query ->
                          let resolve () =
                            Resolver.resolve_external resolver (dst, port) query
                            >>= fun (ttl, answer) ->
                            reply ~content_type:"application/dns-message"
                              ~headers:
                                [ ("cache-control", Fmt.str "max-age=%lu" ttl) ]
                              reqd answer;
                            Lwt.return_unit
                          in
                          Lwt.async resolve
                      | Error (`Msg msg) ->
                          Logs.debug (fun m ->
                              m "couldn't decode query %S: %s" query msg);
                          let headers = H2.Headers.of_list security_headers in
                          let resp = H2.Response.create ~headers `Bad_request in
                          Reqd.respond_with_string reqd resp
                            "Failed to decode query")
                  | `POST (_content_type, data), path
                    when String.starts_with ~prefix:"/dns-query" path ->
                      let resolve () =
                        Resolver.resolve_external resolver (dst, port) data
                        >>= fun (ttl, answer) ->
                        reply ~content_type:"application/dns-message"
                          ~headers:
                            [ ("cache-control", Fmt.str "max-age=%lu" ttl) ]
                          reqd answer;
                        Lwt.return_unit
                      in
                      Lwt.async resolve
                  | _ ->
                      let headers = H2.Headers.of_list security_headers in
                      let resp = H2.Response.create ~headers `Not_found in
                      Reqd.respond_with_string reqd resp "")
              | Error status ->
                  let headers = H2.Headers.of_list security_headers in
                  let resp = H2.Response.create ~headers status in
                  Reqd.respond_with_string reqd resp "")

    let handler t resolver mvar js_file password =
      let query_buffer = ref [] in
      Lwt.async (fun () ->
          let rec record () =
            Lwt_condition.wait (Resolver.queries resolver) >>= fun query ->
            let max_qb_len = 10 in
            let rest =
              if List.length !query_buffer >= max_qb_len then
                List.tl !query_buffer
              else !query_buffer
            in
            query_buffer := rest @ [ query ];
            record ()
          in
          record ());
      {
        Alpn.error;
        request =
          (fun flow dst reqd protocol ->
            request t resolver query_buffer mvar flow dst reqd protocol js_file
              password);
      }

    let update_blocklist http_client resolver =
      let serial = ref 0l in
      let update_one serial source =
        let ingest resp acc data =
          if H2.Status.is_successful resp.Http_mirage_client.status then
            let acc = Angstrom.Buffered.feed acc (`String data) in
            Lwt.return acc
          else Lwt.return acc
        in
        let trie = Resolver.primary_data resolver in
        let trie = Blocklist.remove_old_serial trie source serial in
        let parse_state =
          Angstrom.Buffered.parse (Blocklist_parser.lines source serial trie)
        in
        Http_mirage_client.request http_client source ingest parse_state
        >>= function
        | Error e ->
            Logs.warn (fun m ->
                m "Failed retrieving blocklist from %S: %a" source
                  Mimic.pp_error e);
            Lwt.return `Network_error
        | Ok (resp, acc) ->
            if H2.Status.is_successful resp.status then (
              let acc = Angstrom.Buffered.feed acc `Eof in
              match Angstrom.Buffered.state_to_result acc with
              | Error e ->
                  Logs.warn (fun m ->
                      m "Error parsing blocklist from %S: %s" source e);
                  Lwt.return `Parsing_error
              | Ok trie ->
                  Resolver.update_primary_data resolver trie;
                  Lwt.return `Success)
            else
              let () =
                Logs.warn (fun m ->
                    m "Failed retrieving blocklist from %S: %a" source
                      H2.Status.pp_hum resp.status)
              in
              Lwt.return `Http_error
      in
      let mvar = Lwt_mvar.create `Update in
      let retry = function
        | [] -> fst (Lwt.wait ())
        | _ :: _ as to_retry ->
            Mirage_sleep.ns (Duration.of_sec 30) >>= fun () ->
            Lwt_list.iter_s
              (fun (serial, source) -> update_one serial source >|= ignore)
              to_retry
            >|= fun () -> `Done_retrying
      in
      let one_week () =
        Mirage_sleep.ns (Duration.of_day 7) >|= fun () -> `Update
      in
      let rec loop sources to_retry timer =
        Lwt.pick
          [
            (Lwt_mvar.take mvar :> [> `Done_retrying ] Lwt.t);
            retry to_retry;
            timer;
          ]
        >>= function
        | `Done_retrying -> loop sources [] timer
        | `Update ->
            serial := Int32.succ !serial;
            Lwt_list.map_s (update_one !serial) sources >>= fun status ->
            let to_retry =
              List.filter_map
                (function
                  | source, `Network_error -> Some (!serial, source)
                  | source, `Http_error -> Some (!serial, source)
                  | _ -> None)
                (List.combine sources status)
            in
            Lwt.cancel timer;
            loop sources to_retry (one_week ())
      in
      (mvar, loop (K.dns_blocklist ()) [] (one_week ()))

    let start_resolver t resolver stack tcp http_client js_file password =
      let fresh_tls () =
        let ca = CA.make (K.hostname ()) (Option.get (K.ca_key ())) in
        let cert, pk = Result.get_ok ca in
        let own_cert = `Single ([ cert ], pk) in
        let dns_tls =
          Tls.Config.server ~certificates:own_cert () |> Result.get_ok
        in
        let tls =
          Tls.Config.server ~alpn_protocols:[ "h2"; "http/1.1" ]
            ~certificates:own_cert ()
        in
        let tls = Result.get_ok tls in
        (cert, dns_tls, tls)
      in
      let rec go (certificate, dns_tls, tls) mvar =
        let seven_days_before_expire =
          let _, expiring = X509.Certificate.validity certificate in
          let diff = Ptime.diff expiring (Mirage_ptime.now ()) in
          let days, _ = Ptime.Span.to_d_ps diff in
          max (Duration.of_hour 1) (Duration.of_day (max 0 (days - 7)))
        in
        let stop = Lwt_switch.create () in
        let bell () =
          Mirage_sleep.ns seven_days_before_expire >>= fun () ->
          Lwt_switch.turn_off stop
        in
        Resolver.update_tls resolver dns_tls;
        let h2 =
          HTTP.alpn_service ~tls (handler t resolver mvar js_file password)
        in
        HTTP.init ~port:(K.https_port ()) tcp >>= fun service ->
        let (`Initialized th) = HTTP.serve ~stop h2 service in
        (* Due to the Lwt_switch [stop] [bell] will shut down the web server so
           we can safely wait for both. *)
        Lwt.both (bell ()) th >>= fun _ -> go (fresh_tls ()) mvar
      in
      if K.no_tls () then
        let mvar, th = update_blocklist http_client resolver in
        th
      else
        let cert, dns_tls, tls = fresh_tls () in
        Resolver.update_tls resolver dns_tls;
        let mvar, th = update_blocklist http_client resolver in
        Lwt.join [ th; go (cert, dns_tls, tls) mvar ]
  end

  module Dhcp_dns (Resolver : Dns_resolver_mirage_shared.S) = struct
    let update_dns tcp lease name =
      match (K.dns_key (), K.dns_server ()) with
      | Some (key_name, key), Some ip ->
          let open Dns in
          let key_domain = Domain_name.drop_label_exn ~amount:2 key_name in
          if Domain_name.is_subdomain ~subdomain:name ~domain:key_domain then
            let update =
              let actions =
                let a =
                  (3600l, Ipaddr.V4.Set.singleton lease.Dhcp_server.Lease.addr)
                in
                Packet.Update.[ Remove Rr_map.(K A); Add Rr_map.(B (A, a)) ]
              in
              let update = Domain_name.Map.singleton name actions in
              (Domain_name.Map.empty, update)
            in
            let zone = Packet.Question.create key_domain Rr_map.Soa
            and header =
              (Randomconv.int16 Mirage_crypto_rng.generate, Packet.Flags.empty)
            in
            let packet = Packet.create header zone (`Update update) in
            match
              Dns_tsig.encode_and_sign ~proto:`Tcp packet (Mirage_ptime.now ())
                key key_name
            with
            | Error s ->
                Logs.err (fun m ->
                    m "Error %a while encoding and signing %a" Dns_tsig.pp_s s
                      Domain_name.pp name);
                Lwt.return []
            | Ok (data, mac) -> (
                S.TCP.create_connection tcp (ip, 53) >>= function
                | Error e ->
                    Logs.err (fun m ->
                        m "cannot reach the DNS server %a: %a" Ipaddr.pp ip
                          S.TCP.pp_error e);
                    Lwt.return []
                | Ok flow -> (
                    let len = Cstruct.create 2 in
                    Cstruct.BE.set_uint16 len 0 (String.length data);
                    S.TCP.write flow
                      (Cstruct.append len (Cstruct.of_string data))
                    >>= function
                    | Error e ->
                        Logs.err (fun m ->
                            m "Failed to write to DNS server %a: %a" Ipaddr.pp
                              ip S.TCP.pp_write_error e);
                        Lwt.return []
                    | Ok () -> (
                        S.TCP.read flow >>= function
                        | Error e ->
                            Logs.err (fun m ->
                                m "Failed to read from DNS server %a: %a"
                                  Ipaddr.pp ip S.TCP.pp_error e);
                            Lwt.return []
                        | Ok `Eof ->
                            Logs.err (fun m ->
                                m
                                  "Expected an answer from DNS server %a, got \
                                   eof"
                                  Ipaddr.pp ip);
                            Lwt.return []
                        | Ok (`Data data) ->
                            if Cstruct.length data >= 2 then
                              let len = Cstruct.BE.get_uint16 data 0 in
                              if Cstruct.length data >= 2 + len then
                                let dns_packet =
                                  Cstruct.to_string ~off:2 ~len data
                                in
                                match
                                  Dns_tsig.decode_and_verify
                                    (Mirage_ptime.now ()) key key_name ~mac
                                    dns_packet
                                with
                                | Error e ->
                                    Logs.err (fun m ->
                                        m
                                          "error %a while decoding nsupdate \
                                           answer %a"
                                          Dns_tsig.pp_e e Domain_name.pp name);
                                    Lwt.return []
                                | Ok (res, _, _) -> (
                                    match
                                      Packet.reply_matches_request
                                        ~request:packet res
                                    with
                                    | Ok `Update_ack ->
                                        Logs.info (fun m ->
                                            m "successfully updated DNS");
                                        let options =
                                          [
                                            Dhcp_wire.Client_fqdn
                                              ([ `Overriden ], name);
                                          ]
                                        in
                                        Lwt.return options
                                    | Ok e ->
                                        Logs.warn (fun m ->
                                            m
                                              "failed to update DNS, \
                                               unexpected reply: %a"
                                              Dns.Packet.pp_reply e);
                                        Lwt.return []
                                    | Error e ->
                                        Logs.err (fun m ->
                                            m "invalid reply %a for %a, got %a"
                                              Packet.pp_mismatch e Packet.pp
                                              packet Packet.pp res);
                                        Lwt.return [])
                              else (
                                Logs.warn (fun m ->
                                    m "received short DNS reply");
                                Lwt.return [])
                            else (
                              Logs.warn (fun m ->
                                  m "received a too short DNS reply");
                              Lwt.return []))))
          else (
            Logs.warn (fun m ->
                m "Requested a DNS update with %a, but key domain is %a"
                  Domain_name.pp name Domain_name.pp key_domain);
            Lwt.return [])
      | None, _ ->
          Logs.info (fun m -> m "no DNS key provided");
          Lwt.return []
      | _, None ->
          Logs.info (fun m -> m "no DNS server IP provided");
          Lwt.return []

    let update_tlstunnel tcp lease name =
      match Domain_name.host name with
      | Error (`Msg msg) ->
          Logs.err (fun m ->
              m "cannot construct a host from %a" Domain_name.pp name);
          Lwt.return []
      | Ok name -> (
          match (K.tlstunnel_key (), K.tlstunnel_server ()) with
          | Some secret, Some ip -> (
              let cmd =
                Tlstunnel.Add (name, Ipaddr.V4 lease.Dhcp_server.Lease.addr, 80)
              in
              let data = Tlstunnel.cmd_to_str cmd in
              S.TCP.create_connection tcp (ip, 1234) >>= function
              | Error e ->
                  Logs.err (fun m ->
                      m "cannot reach the TLSTUNNEL server %a: %a" Ipaddr.pp ip
                        S.TCP.pp_error e);
                  Lwt.return []
              | Ok flow -> (
                  S.TCP.write flow (Cstruct.of_string data) >>= function
                  | Error e ->
                      Logs.err (fun m ->
                          m "Failed to write to TLSTUNNEL server %a: %a"
                            Ipaddr.pp ip S.TCP.pp_write_error e);
                      Lwt.return []
                  | Ok () -> (
                      S.TCP.read flow >|= function
                      | Error e ->
                          Logs.err (fun m ->
                              m "Failed to read from TLSTUNNEL server %a: %a"
                                Ipaddr.pp ip S.TCP.pp_error e);
                          []
                      | Ok `Eof ->
                          Logs.err (fun m ->
                              m
                                "Expected an answer from TLSTUNNEL server %a, \
                                 got eof"
                                Ipaddr.pp ip);
                          []
                      | Ok (`Data data) ->
                          (match
                             Tlstunnel.cmd_of_str (Cstruct.to_string data)
                           with
                          | Ok (Result (0, _)) ->
                              Logs.app (fun m ->
                                  m "recorded %a in TLSTUNNEL" Domain_name.pp
                                    name)
                          | Ok (Result (n, msg)) ->
                              Logs.err (fun m ->
                                  m "failed to record %a in TLSTUNNEL: %u (%s)"
                                    Domain_name.pp name n msg)
                          | Ok r ->
                              Logs.err (fun m ->
                                  m "Expected a result from TLSTUNNEL, got %a"
                                    Tlstunnel.pp_cmd r)
                          | Error (`Msg msg) ->
                              Logs.err (fun m ->
                                  m "error while recording %a in TLSTUNNEL: %s"
                                    Domain_name.pp name msg));
                          [])))
          | None, _ ->
              Logs.info (fun m -> m "no TLSTUNNEL key provided");
              Lwt.return []
          | _, None ->
              Logs.info (fun m -> m "no TLSTUNNEL server IP provided");
              Lwt.return [])

    let dhcp_lease_cb tcp resolver domain lease options =
      (match
         ( List.find_opt
             (function Dhcp_wire.Hostname _ -> true | _ -> false)
             options,
           domain )
       with
      | Some (Hostname name), Some domain -> (
          match Domain_name.prepend_label domain name with
          | Ok fqdn ->
              Logs.info (fun m -> m "recording %a" Domain_name.pp fqdn);
              let ptr_name =
                Ipaddr.V4.to_domain_name lease.Dhcp_server.Lease.addr
              in
              let trie = Resolver.primary_data resolver in
              let a_record =
                (3600l, Ipaddr.V4.Set.singleton lease.Dhcp_server.Lease.addr)
              in
              let ptr_record = (3600l, Domain_name.host_exn fqdn) in
              let trie = Dns_trie.insert fqdn Dns.Rr_map.A a_record trie in
              let trie =
                Dns_trie.insert ptr_name Dns.Rr_map.Ptr ptr_record trie
              in
              Resolver.update_primary_data resolver trie
          | Error (`Msg msg) ->
              Logs.warn (fun m ->
                  m "couldn't construct a domain name from %S and %a: %s" name
                    Domain_name.pp domain msg))
      | None, _ ->
          Logs.info (fun m -> m "no Hostname found in the DHCP request")
      | _, None ->
          Logs.info (fun m ->
              m "no domain provided (via --domain), not registering any names")
      | _ ->
          Logs.info (fun m ->
              m "no Hostname or nor domain found in the DHCP request"));
      (match
         List.find_opt
           (function Dhcp_wire.Client_fqdn _ -> true | _ -> false)
           options
       with
        | Some (Dhcp_wire.Client_fqdn (flags, name)) ->
            if List.mem `Server_A flags && not (List.mem `No_update flags) then
              update_dns tcp lease name >>= fun r ->
              update_tlstunnel tcp lease name >>= fun r2 -> Lwt.return (r @ r2)
            else Lwt.return []
        | None ->
            Logs.info (fun m -> m "no client FQDN requested");
            Lwt.return []
        | Some _ -> assert false)
      >>= fun options -> Lwt.return (Ok options)
  end

  let start net assets =
    let mac = N.mac net in
    let configuration, dhcp_config, domain, no_hosts, dnssec, add_reserved =
      match of_commandline mac () with
      | Ok r -> r
      | Error e ->
          Logs.err (fun m -> m "Bad configuration: %s" e);
          exit Mirage_runtime.argument_error
    in
    let net = Net.connect net (Some dhcp_config) in
    ETH.connect net >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    ARP.add_ip arp (Ipaddr.V4.Prefix.address (K.ipv4 ())) >>= fun () ->
    IPV4.connect ~no_init:(K.ipv6_only ()) ~cidr:(K.ipv4 ())
      ?gateway:(K.ipv4_gateway ()) eth arp
    >>= fun ipv4 ->
    IPV6.connect ~no_init:(K.ipv4_only ())
      ~handle_ra:(K.accept_router_advertisements ())
      ?cidr:(K.ipv6 ()) ?gateway:(K.ipv6_gateway ()) net eth
    >>= fun ipv6 ->
    IPV4V6.connect ~ipv4_only:(K.ipv4_only ()) ~ipv6_only:(K.ipv6_only ()) ipv4
      ipv6
    >>= fun ip ->
    ICMP.connect ipv4 >>= fun icmp ->
    UDP.connect ip >>= fun udp ->
    TCP.connect ip >>= fun tcp ->
    S.connect net eth arp ip icmp udp tcp >>= fun stack ->
    HE.connect_device stack >>= fun he ->
    Dns_client.connect (stack, he) >>= fun dns_client ->
    let getaddrinfo v4_or_v6 nam =
      match v4_or_v6 with
      | `A ->
          Dns_client.getaddrinfo dns_client Dns.Rr_map.A nam
          >|= Result.map (fun (_ttl, v4s) ->
              Ipaddr.V4.Set.to_seq v4s
              |> Seq.map (fun v4 -> Ipaddr.V4 v4)
              |> Ipaddr.Set.of_seq)
      | `AAAA ->
          Dns_client.getaddrinfo dns_client Dns.Rr_map.Aaaa nam
          >|= Result.map (fun (_ttl, v6s) ->
              Ipaddr.V6.Set.to_seq v6s
              |> Seq.map (fun v6 -> Ipaddr.V6 v6)
              |> Ipaddr.Set.of_seq)
    in
    HE.inject he getaddrinfo;
    let ctx = Mimic.add Mimic_he.happy_eyeballs he Mimic.empty in
    Http_client.connect ctx >>= fun http_client ->
    let reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    Metrics_lwt.init_periodic (fun () -> Mirage_sleep.ns (Duration.of_sec 10));
    let used_metrics =
      [ "dns-resolver"; "dns-cache"; "dns-resolver-timings"; "memory"; "gc" ]
    in
    List.iter
      (fun src_name ->
        match lookup_src_by_name src_name with
        | None -> Logs.warn (fun m -> m "couldn't find metrics src %s" src_name)
        | Some src -> Metrics.Src.enable src)
      used_metrics;
    let primary_t =
      (* setup DNS server state: *)
      let trie =
        let domain, fqdn =
          match domain with
          | None -> (K.hostname (), K.hostname ())
          | Some d -> (
              match
                ( Domain_name.host d,
                  Result.bind
                    (Domain_name.append (K.hostname ()) d)
                    Domain_name.host )
              with
              | Ok d, Ok fqdn -> (d, fqdn)
              | _, Error (`Msg m) ->
                  Logs.err (fun m ->
                      m "Couldn't figure the FQDN from host %a and domain %a"
                        Domain_name.pp (K.hostname ()) Domain_name.pp d);
                  exit Mirage_runtime.argument_error
              | Error (`Msg m), _ ->
                  Logs.err (fun m ->
                      m "Couldn't use the domain %a as host" Domain_name.pp d);
                  exit Mirage_runtime.argument_error)
        in
        let soa = Dns.Soa.create domain in
        let trie = Dns_trie.insert domain Dns.Rr_map.Soa soa Dns_trie.empty in
        if no_hosts then trie
        else
          let ips = S.IP.configured_ips ip in
          let ipv4s, ipv6s =
            List.fold_left
              (fun (ipv4s, ipv6s) ip ->
                match ip with
                | Ipaddr.V4 v4 ->
                    let v4 = Ipaddr.V4.Prefix.address v4 in
                    (Ipaddr.V4.Set.add v4 ipv4s, ipv6s)
                | Ipaddr.V6 v6 ->
                    let v6 = Ipaddr.V6.Prefix.address v6 in
                    (ipv4s, Ipaddr.V6.Set.add v6 ipv6s))
              (Ipaddr.V4.Set.empty, Ipaddr.V6.Set.empty)
              ips
          in
          let a_record = (3600l, ipv4s) and quad_a_record = (3600l, ipv6s) in
          (if Ipaddr.V4.Set.is_empty ipv4s then trie
           else Dns_trie.insert fqdn Dns.Rr_map.A a_record trie)
          |>
          if Ipaddr.V6.Set.is_empty ipv6s then Fun.id
          else Dns_trie.insert fqdn Dns.Rr_map.Aaaa quad_a_record
      in
      let trie =
        List.fold_left
          (Blocklist.add_single_block "boot-parameter")
          trie (K.dns_block ())
      in
      Dns_server.Primary.create ~trie_cache_entries:0
        ~rng:Mirage_crypto_rng.generate trie
    in
    let password = K.password () in
    (match password with
    | None ->
        Logs.warn (fun m ->
            m
              "No password specified, endpoints requiring authentication won't \
               be accessible.")
    | Some _ -> ());
    (if K.no_tls () then
       Logs.warn (fun m ->
           m
             "TLS is disabled, no HTTPS management interface, and no \
              DNS-over-TLS and DNS-over-HTTPS will be available.")
     else
       match K.ca_key () with
       | None ->
           Logs.err (fun m ->
               m "Neither --no-tls nor --ca-seed specified. %s" K.ca_key_doc);
           exit Mirage_runtime.argument_error
       | Some _ -> ());
    ASSETS.get assets (Mirage_kv.Key.v "main.js") >>= function
    | Error _e -> invalid_arg "JS file could not be loaded"
    | Ok js_file ->
        let qname_min = K.qname_minimisation ()
        and opportunistic = K.opportunistic_tls () in
        (match K.dns_upstream () with
          | None ->
              Logs.info (fun m -> m "using a recursive resolver");
              let module Daemon = Daemon (Resolver) in
              let module Dhcp_dns = Dhcp_dns (Resolver) in
              let resolver =
                let features =
                  (if dnssec then [ `Dnssec ] else [])
                  @ (if qname_min then [ `Qname_minimisation ] else [])
                  @
                  if opportunistic then [ `Opportunistic_tls_authoritative ]
                  else []
                in
                Dns_resolver.create ~add_reserved ?cache_size:(K.dns_cache ())
                  features (Mirage_ptime.now ())
                  (Mirage_mtime.elapsed_ns ())
                  Mirage_crypto_rng.generate primary_t
              in
              let resolver = Resolver.resolver stack ~root:true resolver in
              net.lease_acquired <- Dhcp_dns.dhcp_lease_cb tcp resolver domain;
              let t = { net; mac; configuration } in
              Lwt.async (fun () ->
                  Daemon.start_resolver t resolver stack tcp http_client js_file
                    password);
              Lwt.return_unit
          | Some ns -> (
              Logs.info (fun m ->
                  m "using a stub resolver, forwarding to %s" ns);
              if dnssec then
                Logs.warn (fun m -> m "ignoring DNSSec, using a stub resolver");
              if qname_min then
                Logs.warn (fun m ->
                    m "ignoring qname minimisation, using a stub resolver");
              if opportunistic then
                Logs.warn (fun m ->
                    m "ignoring opportunistic TLS, using a stub resolver");
              let module Daemon = Daemon (Stub) in
              let module Dhcp_dns = Dhcp_dns (Stub) in
              Stub.H.connect_device stack >>= fun happy_eyeballs ->
              try
                Stub.create ~add_reserved ?cache_size:(K.dns_cache ())
                  ~nameservers:[ ns ] primary_t ~happy_eyeballs stack
                >>= fun resolver ->
                net.lease_acquired <- Dhcp_dns.dhcp_lease_cb tcp resolver domain;
                let t = { net; mac; configuration } in
                Lwt.async (fun () ->
                    Daemon.start_resolver t resolver stack tcp http_client
                      js_file password);
                Lwt.return_unit
              with Invalid_argument a ->
                Logs.err (fun m -> m "error %s" a);
                exit Mirage_runtime.argument_error))
        >>= fun () -> S.listen stack
end
