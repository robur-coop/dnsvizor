open Cmdliner
open Dnsvizor

(* 49836 is MirageOS private enterprise number *)
let mirage_pen = 49836l

module Tags = Set.Make (String)
module TagsMap = Map.Make (Tags)

type env = { tcp : Mnet.TCP.state }

type intermediate_config = {
  dhcp_hosts :
    (string list * Macaddr.t list * string * Ipaddr.V4.t option) list;
  dhcp_range : (Ipaddr.V4.t * Ipaddr.V4.t option * int option) option;
  dhcp_options : Dhcp_wire.dhcp_option list;
  tagged_dhcp_options : Dhcp_wire.dhcp_option list TagsMap.t;
  domain : [ `raw ] Domain_name.t option;
  no_hosts : bool;
  dnssec : bool;
  bogus_priv : bool;
}

let merge_tagged t1 t2 =
  let merge _tags o1 o2 =
    match (o1, o2) with
    | None, None -> None
    | Some o, None | None, Some o -> Some o
    | ( Some (o1 : Dhcp_wire.dhcp_option list),
        Some (o2 : Dhcp_wire.dhcp_option list) ) ->
        (* TODO: deduplicate? merge?! *)
        Some (o1 @ o2)
  in
  TagsMap.merge merge t1 t2

module CA = struct
  let prefix =
    X509.Distinguished_name.
      [ Relative_distinguished_name.singleton (CN (Common_name.v "DNSvizor")) ]

  let cacert_dn =
    let open X509.Distinguished_name in
    prefix
    @ [
        Relative_distinguished_name.singleton
          (CN (Common_name.v "Ephemeral CA for DNSvizor"));
      ]

  let cacert_lifetime = Ptime.Span.v (365, 0L)
  let _10s = Ptime.Span.of_int_s 10
  let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
  let reword_error = Result.map_error

  let make domain_name (_, pk) =
    let ( >>= ) = Result.bind in
    let valid_from = Option.get Ptime.(sub_span (Mkernel.now ()) _10s) in
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

module Net = struct
  type nonrec t = {
    net : Mkernel.Net.t;
    mutable config : Dhcp_server.Config.t option;
    mutable leases : Dhcp_server.Lease.database;
    mutable lease_acquired :
      Dhcp_server.Lease.t ->
      pkt:Dhcp_wire.pkt ->
      ours:Dhcp_wire.dhcp_option list ->
      (Dhcp_wire.dhcp_option list, unit) result;
    mutable lease_expired : Dhcp_server.Lease.t -> unit;
  }

  let write t str = Mkernel.Net.write_string t.net str

  let handle_dhcp t config buf =
    match Dhcp_wire.pkt_of_buf buf (Cstruct.length buf) with
    | Error `Not_dhcp -> ()
    | Error (`Msg e) ->
        Logs.err (fun m -> m "Can't parse packet: %s" e);
        ()
    | Ok pkt -> (
        let now = Ptime.to_float_s (Mkernel.now ()) |> Int32.of_float in
        match Dhcp_server.Input.input_pkt config t.leases pkt now with
        | Dhcp_server.Input.Silence -> ()
        | Dhcp_server.Input.Update (lease_opt, leases) ->
            (* if lease_opt is present, the lease got removed! *)
            (match lease_opt with
            | None -> ()
            | Some lease -> t.lease_expired lease);
            t.leases <- leases;
            Logs.debug (fun m ->
                m "Received packet %a - updated lease database" Dhcp_wire.pp_pkt
                  pkt);
            ()
        | Dhcp_server.Input.Warning w ->
            Logs.warn (fun m -> m "%s" w);
            ()
        | Dhcp_server.Input.Error e ->
            Logs.err (fun m -> m "%s" e);
            ()
        | Dhcp_server.Input.Reply (reply, lease_opt, leases) -> (
            let res =
              match lease_opt with
              | None -> Ok reply
              | Some lease -> (
                  Logs.info (fun m ->
                      m "Handing out lease %s, received options %a"
                        (Dhcp_server.Lease.to_string lease)
                        Fmt.(list ~sep:(any ", ") string)
                        (List.map Dhcp_wire.dhcp_option_to_string
                           lease.Dhcp_server.Lease.client_requested_options));
                  match t.lease_acquired lease ~pkt ~ours:reply.options with
                  | Ok options ->
                      let reply = Dhcp_wire.{ reply with options } in
                      Ok reply
                  | Error _ as e -> e)
            in
            match res with
            | Error () -> ()
            | Ok reply -> (
                Logs.info (fun m -> m "Sending reply %a" Dhcp_wire.pp_pkt reply);
                t.leases <- leases;
                try
                  let buf = Dhcp_wire.buf_of_pkt reply in
                  write t (Cstruct.to_string buf)
                with exn ->
                  Logs.err (fun m ->
                      m "got a network error: %s" (Printexc.to_string exn)))))

  let on_packet t (pkt : Slice_bstr.t Ethernet.packet) =
    match (t.config, pkt.Ethernet.protocol) with
    | Some config, Ethernet.IPv4 ->
        let { Slice.buf; off; len } = pkt.Ethernet.payload in
        let frame = Bstr.sub_string buf ~off:0 ~len:(off + len) in
        let cs = Cstruct.of_string frame in
        if Dhcp_wire.is_dhcp cs (Cstruct.length cs) then handle_dhcp t config cs
        else Ethernet.uninteresting_packet ()
    | _ -> Ethernet.uninteresting_packet ()

  let connect net =
    let leases = Dhcp_server.Lease.make_db () in
    let lease_acquired _ ~pkt:_ ~ours:_ = Ok [] in
    let lease_expired _ = () in
    { net; config = None; leases; lease_acquired; lease_expired }

  let disconnect _ =
    Logs.warn (fun m -> m "ignoring disconnect");
    ()

  let mac t = Macaddr.of_octets_exn (Mkernel.Net.mac t.net :> string)
  let mtu t = Mkernel.Net.mtu t.net
end

type t = {
  net : Net.t;
  mac : Macaddr.t;
  ipv4 : Ipaddr.V4.Prefix.t;
  ipv4_gateway : Ipaddr.V4.t option;
  mirage_dhcp :
    Dhcp_wire.dhcp_option list * Dhcp_wire.dhcp_option list TagsMap.t;
  mirage_certify : string list list;
  mutable domain : [ `raw ] Domain_name.t option;
  mutable hosts : Dhcp_server.Config.host list;
  mutable configuration : string;
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
          vendor = None;
        }
      :: r ->
        gather { acc with dhcp_options = option :: acc.dhcp_options } r
    | `Dhcp_option
        {
          option =
            ( Dhcp_wire.Log_servers _ | Dhcp_wire.Vendor_specific _
            | Dhcp_wire.Routers _ | Dhcp_wire.Dns_servers _ ) as option;
          tags = _ :: _ as tags;
          vendor = None;
        }
      :: r ->
        (* If there are multiple tags then *all* tags must be matched. *)
        let tags = Tags.of_list tags in
        let tagged_dhcp_options =
          (* OCaml>=5.1: use TagsMap.add_to_list *)
          TagsMap.update tags
            (function
              | None -> Some [ option ]
              | Some options -> Some (option :: options))
            acc.tagged_dhcp_options
        in
        gather { acc with tagged_dhcp_options } r
    | `Dhcp_option ({ option = _; _ } as dhcp_option) :: _ ->
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
        Error "Don't know how to handle domain with address range or interface"
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
      tagged_dhcp_options = TagsMap.empty;
      domain = None;
      no_hosts = false;
      dnssec = false;
      bogus_priv = false;
    }
    configuration

let dhcp_configuration_of mac config ipv4 ipv4_gateway mirage_dhcp
    mirage_certify =
  let ( let* ) = Result.bind in
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
            (Fmt.str "Don't know what to do with dhcp-range %a,%a" Ipaddr.V4.pp
               start Ipaddr.V4.pp stop)
  in
  let router =
    Option.to_list (Option.map (fun x -> Dhcp_wire.Routers [ x ]) ipv4_gateway)
  and dns_server = [ Dhcp_wire.Dns_servers [ ipv4_address ] ] in
  let mirage_default_options, mirage_hosts = mirage_dhcp in
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
           (fun domain -> Dhcp_wire.Domain_name (Domain_name.to_string domain))
           domain)
    @ mirage_default_options
  in
  let certify_tagged =
    (* The empty string is a dummy value that will be replaced in dhcp_lease_cb *)
    let option = Dhcp_wire.Vi_vendor_info [ (mirage_pen, [ (1, "") ]) ] in
    TagsMap.of_seq
    @@ Seq.map (fun tags -> (Tags.of_list tags, [ option ]))
    @@ List.to_seq mirage_certify
  in
  let tagged_dhcp_options =
    tagged_dhcp_options
    |> merge_tagged certify_tagged
    |> merge_tagged mirage_hosts
  in
  let hosts =
    List.concat_map
      (fun (sets, macs, hostname, fixed_addr) ->
        let sets = Tags.of_list sets in
        List.map
          (fun hw_addr ->
            let options =
              TagsMap.fold
                (fun tags additional_options acc ->
                  (* XXX: do we need to deduplicate?! *)
                  if Tags.subset tags sets then additional_options @ acc
                  else acc)
                tagged_dhcp_options
                (* The [hostname] field doesn't actually do anything in charrua-server -
                        so we add a Hostname dhcp option *)
                (Dhcp_wire.Hostname hostname :: options)
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

let of_commandline mac configuration ipv4 ipv4_gateway mirage_dhcp
    mirage_certify () =
  let ( let+ ) x f = Result.map f x in
  let+ dhcp_configuration, domain, no_hosts, dnssec, bogus_priv =
    dhcp_configuration_of mac configuration ipv4 ipv4_gateway mirage_dhcp
      mirage_certify
  in
  let config =
    Fmt.to_to_string (Dnsvizor.Config_parser.pp_config `File) configuration
  in
  (config, dhcp_configuration, domain, no_hosts, dnssec, bogus_priv)

let update_configuration (t : t) config
    (parsed_config : Dnsvizor.Config_parser.config) =
  let ( let+ ) x f = Result.map f x in
  let+ dhcp_configuration, domain, no_hosts, dnssec, bogus_priv =
    Result.map_error (fun s -> `Msg s)
    @@ dhcp_configuration_of t.mac parsed_config t.ipv4 t.ipv4_gateway
         t.mirage_dhcp t.mirage_certify
  in
  t.configuration <- config;
  t.net.config <- Some dhcp_configuration;
  (domain, no_hosts, dnssec, bogus_priv)

let gen_rev_zones ips =
  List.fold_left
    (fun acc -> function
      | Ipaddr.V4 v4 ->
          let boundary = ((Ipaddr.V4.Prefix.bits v4 + 7) lsr 3) lsl 3 in
          let rev_zones = List.of_seq (Ipaddr.V4.Prefix.subnets boundary v4) in
          let amount = 4 - (boundary lsr 3) in
          List.fold_left
            (fun acc net ->
              let rev_zone =
                Domain_name.drop_label_exn ~amount
                  Ipaddr.V4.(to_domain_name (Prefix.address net))
              in
              rev_zone :: acc)
            acc rev_zones
      | Ipaddr.V6 v6 ->
          let boundary = ((Ipaddr.V6.Prefix.bits v6 + 3) lsr 2) lsl 2 in
          let rev_zones = List.of_seq (Ipaddr.V6.Prefix.subnets boundary v6) in
          let amount = 32 - (boundary lsr 2) in
          List.fold_left
            (fun acc net ->
              let rev_zone =
                Domain_name.drop_label_exn ~amount
                  Ipaddr.V6.(to_domain_name (Prefix.address net))
              in
              rev_zone :: acc)
            acc rev_zones)
    [] ips

let insert_dns_soa trie domain =
  match domain with
  | None -> trie
  | Some domain ->
      let soa = Dns.Soa.create domain in
      Dns_trie.insert domain Dns.Rr_map.Soa soa trie

let insert_dns_rev_soa trie ips =
  List.fold_left
    (fun trie rev_zone ->
      let soa_ptr = Dns.Soa.create rev_zone in
      Dns_trie.insert rev_zone Dns.Rr_map.Soa soa_ptr trie)
    trie (gen_rev_zones ips)

let remove_dns_rev_dns trie ips =
  List.fold_left
    (fun trie rev_zone -> Dns_trie.remove_zone rev_zone trie)
    trie (gen_rev_zones ips)

let remove_dns_soa trie domain =
  match domain with None -> trie | Some zone -> Dns_trie.remove_zone zone trie

let insert_dns_records trie name domain ip =
  match domain with
  | Some domain -> (
      match Domain_name.prepend_label domain name with
      | Ok fqdn ->
          Logs.info (fun m ->
              m "recording %a to %a" Domain_name.pp fqdn Ipaddr.pp ip);
          let trie =
            match ip with
            | Ipaddr.V4 ip ->
                let a_record = (3600l, Ipaddr.V4.Set.singleton ip) in
                Dns_trie.insert fqdn Dns.Rr_map.A a_record trie
            | Ipaddr.V6 ip ->
                let aaaa_record = (3600l, Ipaddr.V6.Set.singleton ip) in
                Dns_trie.insert fqdn Dns.Rr_map.Aaaa aaaa_record trie
          in
          let trie =
            let ptr_record = (3600l, fqdn) in
            let ptr_name = Ipaddr.to_domain_name ip in
            Dns_trie.insert ptr_name Dns.Rr_map.Ptr ptr_record trie
          in
          trie
      | Error (`Msg msg) ->
          Logs.warn (fun m ->
              m "couldn't construct a domain name from %S and %a: %s" name
                Domain_name.pp domain msg);
          trie)
  | None ->
      Logs.info (fun m ->
          m "no domain provided (via --domain), not registering any names");
      trie

let remove_dns_records trie name domain ip =
  match domain with
  | Some domain -> (
      match Domain_name.prepend_label domain name with
      | Ok fqdn ->
          Logs.info (fun m ->
              m "removing %a to %a" Domain_name.pp fqdn Ipaddr.pp ip);
          let trie =
            match ip with
            | Ipaddr.V4 ip ->
                let a_record = (3600l, Ipaddr.V4.Set.singleton ip) in
                Dns_trie.remove fqdn Dns.Rr_map.A a_record trie
            | Ipaddr.V6 ip ->
                let aaaa_record = (3600l, Ipaddr.V6.Set.singleton ip) in
                Dns_trie.remove fqdn Dns.Rr_map.Aaaa aaaa_record trie
          in
          let trie =
            let ptr_record = (3600l, fqdn) in
            let ptr_name = Ipaddr.to_domain_name ip in
            Dns_trie.remove ptr_name Dns.Rr_map.Ptr ptr_record trie
          in
          trie
      | Error (`Msg msg) ->
          Logs.warn (fun m ->
              m "couldn't construct a domain name from %S and %a: %s" name
                Domain_name.pp domain msg);
          trie)
  | None ->
      Logs.info (fun m ->
          m "no domain provided (via --domain), not deregistering any names");
      trie

let lookup_src_by_name name =
  List.find_opt (fun src -> Metrics.Src.name src = name) (Metrics.Src.list ())

let rec clean_dhcp_db (t : Net.t) () =
  Mkernel.sleep 10_000_000_000;
  let now = Ptime.to_float_s (Mkernel.now ()) |> Int32.of_float in
  let db, rm_leases = Dhcp_server.Lease.garbage_collect t.leases ~now in
  t.leases <- db;
  List.iter t.lease_expired rm_leases;
  clean_dhcp_db t ()

let rng =
  let fn () =
    Mirage_crypto_rng_mkernel.(initialize (module Mirage_crypto_rng.Fortuna))
  in
  Mkernel.map fn Mkernel.[] |> Mkernel.finally Mirage_crypto_rng_mkernel.kill

let http =
  let h1 =
    H1.Config.
      {
        default with
        read_buffer_size = 0x4000;
        response_buffer_size = 0x4000;
        response_body_buffer_size = 0x4000;
      }
  in
  `H1 h1

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
          "A static dhcp-host entry, containing the MAC address, hostname, and \
           IPv4 address."
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
           IP ranges which are not found as DHCP lease are answered with \"no \
           such domain\". See RFC 6303, 6761 and 6762 for the address ranges \
           and domains affected."
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
    Arg.(value & opt (some (Config_parser.ignore_c "listen-address")) None doc)

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
          "Should be set when DNSvizor is definitively the only DHCP server on \
           a network. Assumed by charrua, our DHCP server implementation."
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

  let dnsmasq_ignored =
    let open Term.Syntax in
    let+ _ = interface
    and+ _ = except_interface
    and+ _ = listen_address
    and+ _ = no_dhcp_interface
    and+ _ = bind_interfaces
    and+ _ = dhcp_authoritative
    and+ _ = domain_needed in
    ()

  let dhcp_config =
    let open Term.Syntax in
    let+ () = dnsmasq_ignored
    and+ dhcp_range = dhcp_range
    and+ dhcp_host = dhcp_host
    and+ dhcp_options = dhcp_option
    and+ domain = domain
    and+ no_hosts = no_hosts
    and+ dnssec = dnssec
    and+ bogus_priv = bogus_priv in
    (dhcp_host, dhcp_range, dhcp_options, domain, no_hosts, dnssec, bogus_priv)

  let dnsmasq dhcp_config : Config_parser.config =
    let dhcp_host, dhcp_range, dhcp_option, domain, no_hosts, dnssec, bogus_priv
        =
      dhcp_config
    in
    let ( @? ) x xs = match x with None -> xs | Some x -> x :: xs in
    Option.map (fun x -> `Dhcp_range x) dhcp_range
    @? Option.map (fun x -> `Domain x) domain
    @? (if no_hosts then Some `No_hosts else None)
    @? (if dnssec then Some `Dnssec else None)
    @? (if bogus_priv then Some `Bogus_priv else None)
    @? List.map (fun x -> `Dhcp_option x) dhcp_option
    @ List.map (fun x -> `Dhcp_host x) dhcp_host
end

let static_asset_response ~etag =
  let etag = Fmt.str {|"%s"|} (Ohex.encode etag) in
  fun ~mime ~content req (_server : Vifu.Server.t) _env ->
    let open Vifu.Response.Syntax in
    let hdrs = Vifu.Request.headers req in
    match Vifu.Headers.get hdrs "if-none-match" with
    | Some etag' when String.equal etag etag' ->
        let* () = Vifu.Response.with_string req "" in
        Vifu.Response.respond `Not_modified
    | _ ->
        let* () = Vifu.Response.add ~field:"Content-Type" mime in
        let* () = Vifu.Response.add ~field:"ETag" etag in
        let* () = Vifu.Response.with_string req content in
        Vifu.Response.respond `OK

let routes () =
  let open Vifu.Route in
  let open Vifu.Uri in
  [
    get (rel / "main.js" /?? nil)
    --> static_asset_response ~mime:"text/javascript;charset=utf-8"
          ~content:Assets.main_js ~etag:Etag.main_js;
  ]

let run stack _dns_cache _dns_upstream _dns_blocklist _dns_block
    _strip_clone_from_hostname _accept_router_advertisements _hostname
    _tlstunnel_server _tlstunnel_key _dns_server _dns_key _ca_key _no_tls
    _opportunistic_tls _qname_minimisation _mirage_dhcp dhcp_config
    _mirage_certify _name _password https_port =
  Mkernel.(run [ rng; stack ]) @@ fun _rng (_, tcp, _udp) () ->
  let config = Dnsmasq.dnsmasq dhcp_config in
  let cfg = Vifu.Config.v ~http https_port in
  let env = { tcp } in
  Vifu.run ~cfg tcp (routes ()) env

let accept_router_advertisements =
  let doc =
    Arg.info ~docs:Manpage.s_none
      ~doc:
        "Controls whether to listen to and process Router Advertisement (RA) \
         messages sent by other routers on the local network for IPv6"
      [ "accept-router-advertisements" ]
  in
  Arg.(value & flag doc)

let dns_cache =
  let doc = Arg.info ~doc:"DNS cache size" [ "dns-cache" ] in
  Arg.(value & opt (some int) None doc)

let dns_upstream =
  let doc =
    Arg.info
      ~doc:
        "Upstream DNS resolver (if specified, a stub resolver is used instead \
         of a recursive)"
      [ "dns-upstream" ]
  in
  Arg.(value & opt (some string) None doc)

let dns_blocklist =
  let doc =
    Arg.info ~doc:"A web address to fetch DNS block lists from."
      [ "dns-blocklist-url" ]
  in
  Arg.(value & opt_all string [] doc)

let dns_block =
  let doc = Arg.info ~doc:"A domain to block." [ "dns-block" ] in
  let domain = Arg.conv (Domain_name.of_string, Domain_name.pp) in
  Arg.(value & opt_all domain [] doc)

let strip_clone_from_hostname =
  let doc =
    Arg.info
      ~doc:
        "When registering hostnames to DNS, strip \"-clone-*\" from the \
         hostname. This is for interoperation with mollymawk scaling up."
      [ "strip-clone-from-hostname" ]
  in
  Arg.(value & flag doc)

module Dhcp_ext = struct
  let docs = "MIRAGE-SPECIFIC DHCP OPTIONS"

  let mirage_metrics_sink =
    let doc =
      let doc = "Mirage metrics sink." in
      Arg.info ~doc ~docs ~docv:Config_parser.mirage_metrics_sink_docv
        [ "mirage-metrics-sink" ]
    in
    Arg.(value & opt_all Config_parser.mirage_metrics_sink_c [] doc)

  let mirage_vivso =
    let doc =
      let doc =
        "Mirage Vendor-Identifying vendor-specific option. This uses the \
         MirageOS private enterprise number 49836."
      in
      Arg.info ~doc ~docs ~docv:Config_parser.mirage_vivso_docv
        [ "mirage-vivso" ]
    in
    Arg.(value & opt_all Config_parser.mirage_vivso_c [] doc)

  let mirage_certify =
    let doc =
      let doc =
        "Mirage dns-certify. Allow hosts matching the given tags to acquire a \
         certificate using  ACME DNS-01 challenge. The format is: macaddr."
      in
      Arg.info ~doc ~docs ~docv:Config_parser.mirage_certify_docv
        [ "mirage-certify" ]
    in
    Arg.(value & opt_all Dnsvizor.Config_parser.mirage_certify_c [] doc)
end

let mirage_dhcp =
  let open Cmdliner.Term.Syntax in
  let+ metrics_sinks = Dhcp_ext.mirage_metrics_sink
  and+ vivso = Dhcp_ext.mirage_vivso in
  let metrics_sink_default, metrics_sinks =
    let option sink =
      Dhcp_wire.Vi_vendor_info [ (mirage_pen, [ (0, sink) ]) ]
    in
    List.fold_left
      (fun (default, tagged) -> function
        | { Dnsvizor.Config_parser.tags = []; sink } ->
            (option sink :: default, tagged)
        | { tags = _ :: _ as tags; sink } ->
            let tags = Tags.of_list tags in
            ( default,
              TagsMap.update tags
                (function
                  | None -> Some [ option sink ]
                  | Some options -> Some (option sink :: options))
                tagged ))
      ([], TagsMap.empty) metrics_sinks
  in
  let default_vivso, vivso =
    let option code data =
      Dhcp_wire.Vi_vendor_info [ (mirage_pen, [ (code, data) ]) ]
    in
    List.fold_left
      (fun (default, tagged) -> function
        | { Dnsvizor.Config_parser.tags = []; subopt_code; subopt_data } ->
            (option subopt_code subopt_data :: default, tagged)
        | { tags = _ :: _ as tags; subopt_code; subopt_data } ->
            let tags = Tags.of_list tags in
            ( default,
              TagsMap.update tags
                (function
                  | None -> Some [ option subopt_code subopt_data ]
                  | Some options ->
                      Some (option subopt_code subopt_data :: options))
                tagged ))
      ([], TagsMap.empty) vivso
  in
  (metrics_sink_default @ default_vivso, merge_tagged metrics_sinks vivso)

let qname_minimisation =
  let doc =
    Arg.info ~doc:"Use qname minimisation (RFC 9156)." [ "qname-minimisation" ]
  in
  Arg.(value & flag doc)

let opportunistic_tls =
  let doc =
    Arg.info
      ~doc:
        "Use opportunistic TLS from recursive resolver to authoriative (RFC \
         9539)."
      [ "opportunistic-tls-authoritative" ]
  in
  Arg.(value & flag doc)

let https_port =
  let doc = Arg.info ~doc:"The HTTPS port." [ "https-port" ] in
  Arg.(value & opt int 443 & doc)

let no_tls =
  let doc =
    Arg.info ~doc:"Disable TLS (HTTPS server and DNS-over-TLS/DNS-over-HTTPS)"
      [ "no-tls" ]
  in
  Arg.(value & flag doc)

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
  Arg.(
    value
    & opt (some (conv (parser, pp))) None
    & info [ "ca-seed" ] ~doc:ca_key_doc)

let password =
  let doc = Arg.info ~doc:"Password used for authentication" [ "password" ] in
  Arg.(value & opt (some string) None doc)

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
  Arg.(value & opt (some key_v) None & info ~doc [ "dns-key" ])

let ipaddr = Arg.conv (Ipaddr.of_string, Ipaddr.pp)

let dns_server =
  let doc = "The IP address of the DNS server to update with new host names" in
  Arg.(value & opt (some ipaddr) None & info ~doc [ "dns-server" ])

let tlstunnel_key =
  let doc =
    "The shared secret of the TLSTUNNEL server to update with new host names"
  in
  Arg.(value & opt (some string) None & info ~doc [ "tlstunnel-key" ])

let tlstunnel_server =
  let doc = "The IP address of the TLSTUNNEL server" in
  Arg.(value & opt (some ipaddr) None & info ~doc [ "tlstunnel-server" ])

let name =
  let doc = "The name of the unikernel" in
  let open Arg in
  value & opt string "dnsvizor" & info [ "name" ] ~doc ~docv:"NAME"

let term =
  let open Term in
  const (fun _ -> run)
  $ Mnet_cli.setup_logs $ Mnet_cli.setup "service" $ dns_cache $ dns_upstream
  $ dns_blocklist $ dns_block $ strip_clone_from_hostname
  $ accept_router_advertisements $ hostname $ tlstunnel_server $ tlstunnel_key
  $ dns_server $ dns_key $ ca_key $ no_tls $ opportunistic_tls
  $ qname_minimisation $ mirage_dhcp $ Dnsmasq.dhcp_config
  $ Dhcp_ext.mirage_certify $ name $ password $ https_port

let cmd =
  let info = Cmd.info "dnsvizor" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
