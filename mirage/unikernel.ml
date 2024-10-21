open Lwt.Infix

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

  (* TODO support multiple dhcp-range statements *)
  let dhcp_range =
    let doc = Arg.info ~doc:"Enable DHCP server." [ "dhcp-range" ] in
    Mirage_runtime.register_arg
      Arg.(value & opt Config_parser.(some dhcp_range_c) None doc)

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
end

module Main
    (R : Mirage_crypto_rng_mirage.S)
    (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S)
    (N : Mirage_net.S) =
struct
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
          let now = M.elapsed_ns () |> Duration.to_sec |> Int32.of_int in
          match Dhcp_server.Input.input_pkt config t.dhcp_leases pkt now with
          | Dhcp_server.Input.Silence -> Lwt.return_unit
          | Dhcp_server.Input.Update leases ->
              t.dhcp_leases <- leases;
              Logs.debug (fun m ->
                  m "Received packet %s - updated lease database"
                    (Dhcp_wire.pkt_to_string pkt));
              Lwt.return_unit
          | Dhcp_server.Input.Warning w ->
              Logs.warn (fun m -> m "%s" w);
              Lwt.return_unit
          | Dhcp_server.Input.Error e ->
              Logs.err (fun m -> m "%s" e);
              Lwt.return_unit
          | Dhcp_server.Input.Reply (reply, leases) ->
              t.dhcp_leases <- leases;
              Logs.debug (fun m ->
                  m "Received packet %s" (Dhcp_wire.pkt_to_string pkt));
              N.write t.net
                ~size:(N.mtu t.net + Ethernet.Packet.sizeof_ethernet)
                (Dhcp_wire.pkt_into_buf reply)
              >|= fun _ ->
              Logs.debug (fun m ->
                  m "Sent reply packet %s" (Dhcp_wire.pkt_to_string reply)))

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
  module ARP = Arp.Make (ETH) (Time)
  module IPV4 = Static_ipv4.Make (R) (M) (ETH) (ARP)
  module IPV6 = Ipv6.Make (Net) (ETH) (R) (Time) (M)
  module IPV4V6 = Tcpip_stack_direct.IPV4V6 (IPV4) (IPV6)
  module ICMP = Icmpv4.Make (IPV4)
  module UDP = Udp.Make (IPV4V6) (R)
  module TCP = Tcp.Flow.Make (IPV4V6) (Time) (M) (R)

  module S =
    Tcpip_stack_direct.MakeV4V6 (Time) (R) (Net) (ETH) (ARP) (IPV4V6) (ICMP)
      (UDP)
      (TCP)

  module Resolver = Dns_resolver_mirage.Make (R) (P) (M) (Time) (S)
  module Stub = Dns_stub_mirage.Make (R) (Time) (P) (M) (S)

  let start () () () () net =
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
    ICMP.connect ipv4 >>= fun icmp ->
    UDP.connect ip >>= fun udp ->
    TCP.connect ip >>= fun tcp ->
    S.connect net eth arp ip icmp udp tcp >>= fun stack ->
    let primary_t =
      (* setup DNS server state: *)
      Dns_server.Primary.create ~rng:Mirage_crypto_rng.generate Dns_trie.empty
    in
    (match K.dns_upstream () with
    | None ->
        Logs.info (fun m -> m "using a recursive resolver");
        let resolver =
          Dns_resolver.create ?cache_size:(K.dns_cache ()) ~dnssec:false
            (M.elapsed_ns ()) R.generate primary_t
        in
        Resolver.resolver stack ~root:true resolver;
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
