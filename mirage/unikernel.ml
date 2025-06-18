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
    >>= fun certificate ->
    let fingerprint = X509.Certificate.fingerprint `SHA256 certificate in
    let time () = Some (Mirage_ptime.now ()) in
    let authenticator =
      X509.Authenticator.cert_fingerprint ~time ~hash:`SHA256 ~fingerprint
    in
    Ok (certificate, pk, authenticator)
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

  (* DNSmasq configuration options *)
  (* TODO support multiple dhcp-range statements *)
  let dhcp_range =
    let doc =
      Arg.info ~doc:"Enable DHCP server." ~docv:Config_parser.dhcp_range_docv
        [ "dhcp-range" ]
    in
    Mirage_runtime.register_arg
      Arg.(value & opt Config_parser.(some dhcp_range_c) None doc)

  let interface =
    let doc =
      Arg.info ~docs:Manpage.s_none ~doc:"Interface to listen on."
        [ "interface" ]
    in
    Mirage_runtime.register_arg
      Arg.(value & opt Config_parser.(some (ignore_c "interface")) None doc)

  let except_interface =
    let doc =
      Arg.info ~docs:Manpage.s_none ~doc:"Interface to not listen on."
        [ "except-interface" ]
    in
    Mirage_runtime.register_arg
      Arg.(
        value & opt Config_parser.(some (ignore_c "except-interface")) None doc)

  let listen_address =
    let doc =
      Arg.info ~docs:Manpage.s_none ~doc:"IP address to listen on."
        [ "listen-address" ]
    in
    Mirage_runtime.register_arg
      Arg.(
        value & opt Config_parser.(some (ignore_c "listen-address")) None doc)

  let no_dhcp_interface =
    let doc =
      Arg.info ~docs:Manpage.s_none ~doc:"Only provide DNS service on."
        [ "no-dhcp-interface" ]
    in
    Mirage_runtime.register_arg
      Arg.(
        value & opt Config_parser.(some (ignore_c "no-dhcp-interface")) None doc)

  let bind_interfaces =
    let doc =
      Arg.info ~docs:Manpage.s_none ~doc:"Bind to interface IP address only."
        [ "bind_interfaces" ]
    in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let dnssec =
    let doc =
      Arg.info ~doc:"Validate DNS replies and cache DNSSEC data." [ "dnssec" ]
    in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let https_port =
    let doc = Arg.info ~doc:"The HTTPS port." [ "https-port" ] in
    Mirage_runtime.register_arg Arg.(value & opt int 443 & doc)

  let valid_bits str =
    try
      let bits = int_of_string str in
      bits >= 89
    with _ -> false

  let ca_key =
    let doc =
      "The seed (base64 encoded) used to generate the private key for the \
       certificate. The seed can be prepended by the type of the key (rsa or \
       ed25519) plus a colon. For a RSA key, the user can also specify bits: \
       \"rsa:4096:foo=\"."
    in
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
    let arg =
      let open Arg in
      required & opt (some (conv (parser, pp))) None & info [ "ca-seed" ] ~doc
    in
    Mirage_runtime.register_arg arg

  let name =
    let ( let* ) = Result.bind in
    let parser str =
      let* dn = Domain_name.of_string str in
      Domain_name.host dn
    in
    let pp = Domain_name.pp in
    let domain_name = Arg.conv (parser, pp) in
    let doc = "The name (and the SNI for the certificate) of the unikernel." in
    let arg =
      let open Arg in
      required & opt (some domain_name) None & info [ "name" ] ~doc
    in
    Mirage_runtime.register_arg arg
end

module Main (N : Mirage_net.S) (ASSETS : Mirage_kv.RO) = struct
  let js_file assets =
    ASSETS.get assets (Mirage_kv.Key.v "main.js") >|= function
    | Error _e -> invalid_arg "JS file could not be loaded"
    | Ok js -> js

  module Net = struct
    (* A Mirage_net.S implementation which diverts DHCP messages to a DHCP
       server. The DHCP server needs to get the entire Ethernet frame, because
       the Ethernet source address is the address to send replies to, its IPv4
       addresses (source, destination) do not matter (since the DHCP client that
       sent this request does not have an IP address yet). ARP cannot be used
       by DHCP, because the client does not have an IP address (and thus no ARP
       replies). *)

    type error = N.error

    let pp_error = N.pp_error

    type t = {
      net : N.t;
      dhcp_config : Dhcp_server.Config.t option;
      mutable dhcp_leases : Dhcp_server.Lease.database;
    }

    let write t = N.write t.net

    let handle_dhcp t config buf =
      match Dhcp_wire.pkt_of_buf buf (Cstruct.length buf) with
      | Error e ->
          Logs.err (fun m -> m "Can't parse packet: %s" e);
          Lwt.return_unit
      | Ok pkt -> (
          let now =
            Mirage_mtime.elapsed_ns () |> Duration.to_sec |> Int32.of_int
          in
          match Dhcp_server.Input.input_pkt config t.dhcp_leases pkt now with
          | Dhcp_server.Input.Silence -> Lwt.return_unit
          | Dhcp_server.Input.Update leases ->
              t.dhcp_leases <- leases;
              Logs.debug (fun m ->
                  m "Received packet %a - updated lease database"
                    Dhcp_wire.pp_pkt pkt);
              Lwt.return_unit
          | Dhcp_server.Input.Warning w ->
              Logs.warn (fun m -> m "%s" w);
              Lwt.return_unit
          | Dhcp_server.Input.Error e ->
              Logs.err (fun m -> m "%s" e);
              Lwt.return_unit
          | Dhcp_server.Input.Reply (reply, leases) ->
              t.dhcp_leases <- leases;
              Logs.debug (fun m -> m "Received packet %a" Dhcp_wire.pp_pkt pkt);
              N.write t.net
                ~size:(N.mtu t.net + Ethernet.Packet.sizeof_ethernet)
                (Dhcp_wire.pkt_into_buf reply)
              >|= fun _ ->
              Logs.debug (fun m ->
                  m "Sent reply packet %a" Dhcp_wire.pp_pkt reply))

    let listen t ~header_size net =
      let dhcp_or_not buf =
        let of_interest hdr =
          let dst = hdr.Ethernet.Packet.destination in
          Macaddr.compare dst (N.mac t.net) = 0 || not (Macaddr.is_unicast dst)
        in
        match t.dhcp_config with
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

    let connect net ?(dhcp_leases = Dhcp_server.Lease.make_db ()) dhcp_config =
      { net; dhcp_config; dhcp_leases }

    let disconnect _ =
      Logs.warn (fun m -> m "ignoring disconnect");
      Lwt.return_unit

    let mac t = N.mac t.net
    let mtu t = N.mtu t.net
    let get_stats_counters t = N.get_stats_counters t.net
    let reset_stats_counters t = N.reset_stats_counters t.net
  end

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

  module Daemon = struct
    let pp_error ppf = function
      | `Bad_gateway -> Fmt.string ppf "Bad gateway"
      | `Bad_request -> Fmt.string ppf "Bad request"
      | `Exn exn -> Fmt.pf ppf "Exception: %s" (Printexc.to_string exn)
      | `Internal_server_error -> Fmt.string ppf "Internal server error"

    let error :
        type reqd headers request response ro wo.
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

    let web_ui_handler js_file metrics_cache req_method path =
      match (req_method, path) with
      | `GET, "/main.js" -> Some (js_file, Some "text/javascript")
      | `GET, "/" | `GET, "/dashboard" ->
          let map = metrics_cache () in
          let lookup_src_by_name name =
            List.find_opt
              (fun src -> Metrics.Src.name src = name)
              (Metrics.Src.list ())
          in
          let lookup_stats name =
            match lookup_src_by_name name with
            | None -> []
            | Some src -> (
                match Metrics.SM.find_opt src map with
                | None -> []
                | Some (_tags, data) -> Metrics.Data.fields data)
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
          let content =
            Statistics.statistics_page resolv_stats dns_cache_stats
              resolver_timing memory_stats gc_stats
          in
          Some (Dashboard.dashboard_layout ~content (), None)
      | `GET, "/querylog" ->
          Some
            (Dashboard.dashboard_layout ~content:Query_logs.query_page (), None)
      | `GET, "/blocklist" ->
          Some
            (Dashboard.dashboard_layout ~content:Blocklist.block_page (), None)
      | _ -> None

    let request :
        type reqd headers request response ro wo.
        _ ->
        _ ->
        HTTP.TLS.flow ->
        Ipaddr.t * int ->
        reqd ->
        (reqd, headers, request, response, ro, wo) Alpn.protocol ->
        string ->
        unit =
     fun resolver metrics_cache _flow (dst, port) reqd protocol js_file ->
      match protocol with
      | Alpn.HTTP_1_1 (module Reqd) ->
          Lwt.async (fun () ->
              let reply ?(content_type = "text/html") reqd ?(headers = []) data
                  =
                let headers =
                  H1.Headers.of_list
                    ([
                       ("content-type", content_type);
                       ("content-legnth", string_of_int (String.length data));
                     ]
                    @ headers)
                in
                let resp = H1.Response.create ~headers `OK in
                Reqd.respond_with_string reqd resp data;
                Lwt.return_unit
              in
              Logs.info (fun m -> m "Got a new HTTPS request!");
              let request = Reqd.request reqd in
              Logs.info (fun m ->
                  m "%a %s" H1.Method.pp_hum request.H1.Request.meth
                    request.H1.Request.target);
              match
                web_ui_handler js_file metrics_cache request.H1.Request.meth
                  request.H1.Request.target
              with
              | Some (content, content_type) -> reply ?content_type reqd content
              | None ->
                  let headers =
                    H1.Headers.of_list [ ("connection", "close") ]
                  in
                  let resp = H1.Response.create ~headers `Bad_request in
                  Reqd.respond_with_string reqd resp "";
                  Lwt.return_unit
              (* HTTP/2 (RFC7540) is the minimum RECOMMENDED version of HTTP for use
                 with DoH. https://datatracker.ietf.org/doc/html/rfc8484#section-5.2 *))
      | Alpn.H2 (module Reqd) -> (
          let reply ?(content_type = "text/html") reqd ?(headers = []) data =
            let headers =
              H2.Headers.of_list
                ([
                   ("content-type", content_type);
                   ("content-length", string_of_int (String.length data));
                 ]
                @ headers)
            in
            let resp = H2.Response.create ~headers `OK in
            Reqd.respond_with_string reqd resp data
          in
          Logs.info (fun m -> m "Got a new DNS over HTTPS request!");
          let request = Reqd.request reqd in
          Logs.info (fun m ->
              m "%a %s" H2.Method.pp_hum request.H2.Request.meth
                request.H2.Request.target);
          match
            web_ui_handler js_file metrics_cache request.H2.Request.meth
              request.H2.Request.target
          with
          | Some (content, content_type) -> reply ?content_type reqd content
          | None -> (
              match (request.H2.Request.meth, request.H2.Request.target) with
              | `GET, path when String.starts_with ~prefix:"/dns-query" path ->
                  let target = request.H2.Request.target in
                  let elts = String.split_on_char '=' target in
                  let elts = List.tl elts in
                  let query = String.concat "=" elts in
                  Logs.info (fun m -> m "%s" query);
                  let query = Base64.decode_exn ~pad:false query in
                  let resolve () =
                    Resolver.resolve_external resolver (dst, port) query
                    >>= fun (ttl, answer) ->
                    reply ~content_type:"application/dns-message"
                      ~headers:[ ("cache-control", Fmt.str "max-age=%lu" ttl) ]
                      reqd answer;
                    Lwt.return_unit
                  in
                  Lwt.async resolve
              | `POST, path when String.starts_with ~prefix:"/dns-query" path ->
                  let target = request.H2.Request.target in
                  let initial_size =
                    Option.bind
                      (H2.Headers.get request.headers "content-length")
                      int_of_string_opt
                    |> Option.value ~default:65536
                  in
                  Logs.info (fun m -> m ">>> %S" target);
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
                  let resolve () =
                    finished >>= fun data ->
                    Resolver.resolve_external resolver (dst, port) data
                    >>= fun (ttl, answer) ->
                    reply ~content_type:"application/dns-message"
                      ~headers:[ ("cache-control", Fmt.str "max-age=%lu" ttl) ]
                      reqd answer;
                    Lwt.return_unit
                  in
                  Lwt.async resolve
              | _ ->
                  let headers =
                    H2.Headers.of_list [ ("connection", "close") ]
                  in
                  let resp = H2.Response.create ~headers `Bad_request in
                  Reqd.respond_with_string reqd resp ""))

    let handler resolver js_file metrics_cache =
      {
        Alpn.error;
        request =
          (fun flow dst reqd protocol ->
            request resolver metrics_cache flow dst reqd protocol js_file);
      }

    let start_resolver stack tcp resolver js_file metrics_cache =
      let rec go (certificate, pk, _authenticator) =
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
        let own_cert = `Single ([ certificate ], pk) in
        let dns_tls =
          Tls.Config.server ~certificates:own_cert () |> Result.get_ok
        in
        let resolver =
          Resolver.resolver stack ~root:true ~tls:dns_tls resolver
        in
        let tls =
          Tls.Config.server ~alpn_protocols:[ "h2"; "http/1.1" ]
            ~certificates:own_cert ()
        in
        let tls = Result.get_ok tls in
        let h2 =
          HTTP.alpn_service ~tls (handler resolver js_file metrics_cache)
        in
        HTTP.init ~port:(K.https_port ()) tcp >>= fun service ->
        let (`Initialized th) = HTTP.serve ~stop h2 service in
        Lwt.both (bell ()) th >>= fun _ ->
        let ca = CA.make (K.name ()) (K.ca_key ()) in
        go (Result.get_ok ca)
      in
      let ca = CA.make (K.name ()) (K.ca_key ()) in
      go (Result.get_ok ca)
  end

  let start net assets =
    (match K.dhcp_range () with
    | None -> ()
    | Some x ->
        Logs.info (fun m ->
            m "dhcp-range: %a" Dnsvizor.Config_parser.pp_dhcp_range x));
    let v4_address = Ipaddr.V4.Prefix.address (K.ipv4 ()) in
    let mac = N.mac net in
    let dhcp_config =
      match K.dhcp_range () with
      | None -> None
      | Some dhcp_range ->
          let options =
            (match K.ipv4_gateway () with
            | None -> []
            | Some x -> [ Dhcp_wire.Routers [ x ] ])
            @ [ Dhcp_wire.Dns_servers [ v4_address ] ]
            (* Dhcp_wire.Domain_name __ *)
          in
          let range =
            (* doesn't check start < stop *)
            let start = dhcp_range.Dnsvizor.Config_parser.start_addr in
            let stop =
              (* TODO assumes /24 also automatically fills stuff *)
              match dhcp_range.end_addr with
              | Some i -> i
              | None ->
                  Ipaddr.V4.of_int32
                    Int32.(
                      logand 0xfffffffel
                        (logor 0x000000fel (Ipaddr.V4.to_int32 start)))
            in
            Some (start, stop)
          in
          let default_lease_time = dhcp_range.lease_time in
          (* TODO: what should be the max_lease_time? 2 * default_lease_time *)
          Some
            (Dhcp_server.Config.make ?hostname:None ?default_lease_time
               ?max_lease_time:None ?hosts:None ~addr_tuple:(v4_address, mac)
               ~network:(Ipaddr.V4.Prefix.prefix (K.ipv4 ()))
               ~range ~options ())
    in
    let net = Net.connect net dhcp_config in
    ETH.connect net >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    ARP.add_ip arp v4_address >>= fun () ->
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
    js_file assets >>= fun js_file ->
    ICMP.connect ipv4 >>= fun icmp ->
    UDP.connect ip >>= fun udp ->
    TCP.connect ip >>= fun tcp ->
    S.connect net eth arp ip icmp udp tcp >>= fun stack ->
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> Mirage_sleep.ns (Duration.of_sec 10));
    let primary_t =
      (* setup DNS server state: *)
      Dns_server.Primary.create ~rng:Mirage_crypto_rng.generate Dns_trie.empty
    in
    (match K.dns_upstream () with
    | None ->
        Logs.info (fun m -> m "using a recursive resolver");
        let resolver =
          Dns_resolver.create ?cache_size:(K.dns_cache ()) ~dnssec:(K.dnssec ())
            (Mirage_mtime.elapsed_ns ())
            Mirage_crypto_rng.generate primary_t
        in
        Lwt.async (fun () ->
            Daemon.start_resolver stack tcp resolver js_file get_cache);
        Lwt.return_unit
    | Some ns -> (
        Logs.info (fun m -> m "using a stub resolver, forwarding to %s" ns);
        Stub.H.connect_device stack >>= fun happy_eyeballs ->
        try
          Stub.create ?cache_size:(K.dns_cache ()) ~nameservers:[ ns ] primary_t
            ~happy_eyeballs stack
          >|= fun _ -> ()
        with Invalid_argument a ->
          Logs.err (fun m -> m "error %s" a);
          exit Mirage_runtime.argument_error))
    >>= fun () -> S.listen stack
end
