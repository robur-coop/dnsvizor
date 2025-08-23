open Dnsvizor.Config_parser

let msg_t =
  let pp ppf (`Msg s) = Fmt.string ppf s in
  Alcotest.testable pp (fun (`Msg a) (`Msg b) -> String.equal a b)

let opt_eq f a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> f a b
  | None, Some _ | Some _, None -> false

let ipv4_eq a b = Ipaddr.V4.compare a b = 0
let ipv6_eq a b = Ipaddr.V6.compare a b = 0
let mac_eq a b = Macaddr.compare a b = 0

let mode_eq a b =
  match (a, b) with
  | `Static, `Static | `Proxy, `Proxy -> true
  | `Static, _ | `Proxy, _ -> false

let dhcp_range_t =
  let equal a b =
    ipv4_eq a.start_addr b.start_addr
    && opt_eq ipv4_eq a.end_addr b.end_addr
    && opt_eq mode_eq a.mode b.mode
    && opt_eq ipv4_eq a.netmask b.netmask
    && opt_eq ipv4_eq a.broadcast b.broadcast
    && opt_eq Int.equal a.lease_time b.lease_time
  in
  Alcotest.testable pp_dhcp_range equal

let dhcp_host_t =
  let equal
      { id; sets; tags; macs; ipv4; ipv6; lease_time; ignore; domain_name } b =
    Option.equal
      (fun id id' ->
        match (id, id') with
        | `Any_client_id, `Any_client_id -> true
        | `Client_id id, `Client_id id' -> String.equal id id'
        | `Any_client_id, `Client_id _ | `Client_id _, `Any_client_id -> false)
      id b.id
    && List.equal String.equal sets b.sets
    && List.equal String.equal tags b.tags
    && List.equal mac_eq macs b.macs
    && Option.equal ipv4_eq ipv4 b.ipv4
    && Option.equal ipv6_eq ipv6 b.ipv6
    && Option.equal Int.equal lease_time b.lease_time
    && Bool.equal ignore b.ignore
    && Option.equal Domain_name.equal domain_name b.domain_name
  in
  Alcotest.testable pp_dhcp_host equal

let parse_one_arg rule input = parse_one (rule arg_end_of_directive) input

let ok_dhcp_range () =
  let input = "192.168.0.50,192.168.0.150,12h" in
  let expected =
    {
      start_addr = Ipaddr.V4.of_string_exn "192.168.0.50";
      end_addr = Some (Ipaddr.V4.of_string_exn "192.168.0.150");
      mode = None;
      netmask = None;
      broadcast = None;
      lease_time = Some (12 * 60 * 60);
    }
  in
  Alcotest.(
    check
      (result dhcp_range_t msg_t)
      "DHCP range is good" (Ok expected)
      (parse_one_arg dhcp_range input))

let ok_dhcp_range_with_netmask () =
  let input = "192.168.0.50,192.168.0.150,255.255.255.0,12h" in
  let expected =
    {
      start_addr = Ipaddr.V4.of_string_exn "192.168.0.50";
      end_addr = Some (Ipaddr.V4.of_string_exn "192.168.0.150");
      mode = None;
      netmask = Some (Ipaddr.V4.of_string_exn "255.255.255.0");
      broadcast = None;
      lease_time = Some (12 * 60 * 60);
    }
  in
  Alcotest.(
    check
      (result dhcp_range_t msg_t)
      "DHCP range with netmask is good" (Ok expected)
      (parse_one_arg dhcp_range input))

let ok_dhcp_range_static () =
  (* NOTE: there's no netmask, with dnsmasq it comes from the configured
     interfaces. Unclear whether we want to support this without netmask. *)
  let input = "192.168.0.0,static" in
  let expected =
    {
      start_addr = Ipaddr.V4.of_string_exn "192.168.0.0";
      end_addr = None;
      mode = Some `Static;
      netmask = None;
      broadcast = None;
      lease_time = None;
    }
  in
  Alcotest.(
    check
      (result dhcp_range_t msg_t)
      "DHCP range with static is good" (Ok expected)
      (parse_one_arg dhcp_range input))

let make_dhcp_host ?id ?(sets = []) ?(tags = []) ?(macs = []) ?ipv4 ?ipv6
    ?lease_time ?(ignore = false) ?domain_name () =
  { id; sets; tags; macs; ipv4; ipv6; lease_time; ignore; domain_name }

let ok_dhcp_host_thedoctor () =
  let input = "00:00:5e:00:53:42,thedoctor,192.168.0.10" in
  let expected =
    make_dhcp_host
      ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:42" ]
      ~domain_name:(Domain_name.of_string_exn "thedoctor")
      ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.10")
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host thedoctor is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_tardis () =
  let input = "00:00:5e:00:53:01,00:00:5e:00:53:02,tardis,192.168.0.22" in
  let expected =
    make_dhcp_host
      ~macs:
        Macaddr.
          [
            of_string_exn "00:00:5e:00:53:01"; of_string_exn "00:00:5e:00:53:02";
          ]
      ~domain_name:(Domain_name.of_string_exn "tardis")
      ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.22")
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host tardis is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_sonicscrewdriver () =
  let input = "00:00:5e:00:53:08,sonicscrewdriver,192.168.0.23" in
  let expected =
    make_dhcp_host
      ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:08" ]
      ~domain_name:(Domain_name.of_string_exn "sonicscrewdriver")
      ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.23")
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host sonicscrewdriver is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_apollon () =
  let input = "a2:54:00:42:6a:43,apollon,10.10.10.51,infinite" in
  let expected =
    make_dhcp_host
      ~macs:[ Macaddr.of_string_exn "a2:54:00:42:6a:43" ]
      ~domain_name:(Domain_name.of_string_exn "apollon")
      ~ipv4:(Ipaddr.V4.of_string_exn "10.10.10.51")
      ~lease_time:(1 lsl 32) (* infinite *)
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host apollon is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_dnsmasq_conf_example =
  let to_test (name, input, expected) =
    ( name,
      `Quick,
      fun () ->
        Alcotest.(check (result dhcp_host_t msg_t))
          (name ^ " is good") (Ok expected)
          (parse_one_arg dhcp_host input) )
  in
  List.map to_test
    [
      ( "First dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,192.168.0.60",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      ( "Second dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,fred",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~domain_name:(Domain_name.of_string_exn "fred")
          () );
      ( "Third dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,fred,192.168.0.60,45m",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~domain_name:(Domain_name.of_string_exn "fred")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          ~lease_time:(45 * 60) () );
      ( "Fourth dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,12:34:56:78:90:12,192.168.0.60",
        make_dhcp_host
          ~macs:
            Macaddr.
              [
                of_string_exn "11:22:33:44:55:66";
                of_string_exn "12:34:56:78:90:12";
              ]
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      ( "Fifth dhcp-host from dnsmasq.conf.example",
        "bert,192.168.0.70,infinite",
        make_dhcp_host
          ~domain_name:(Domain_name.of_string_exn "bert")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.70")
          ~lease_time:(1 lsl 32) () );
      ( "Sixth dhcp-host from dnsmasq.conf.example",
        "id:01:02:02:04,192.168.0.60",
        make_dhcp_host ~id:(`Client_id "\x01\x02\x02\x04")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      (* skipping seventh as it's similar to above, but is infiniband in a manner we wouldn't care about *)
      ( "Eigth dhcp-host from dnsmasq.conf.example",
        "id:marjorie,192.168.0.60",
        make_dhcp_host ~id:(`Client_id "marjorie")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      ( "Ninth dhcp-host from dnsmasq.conf.example",
        "judge",
        make_dhcp_host ~domain_name:(Domain_name.of_string_exn "judge") () );
      ( "Tenth dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,ignore",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~ignore:true () );
      ( "Eleventh dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,id:*",
        make_dhcp_host ~id:`Any_client_id
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          () );
      ( "Twelvth dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,set:red",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~sets:[ "red" ] () );
      (* TODO:
         ("Thirteenth dhcp-host from dnsmasq.conf.example",
           "11:22:33:*:*:*,set:red",
           _);
         ("Fourteenth dhcp-host from dnsmasq.conf.example",
           "id:00:01:00:01:16:d2:83:fc:92:d4:19:e2:d8:b2, fred, [1234::5]",
           _);
      *)
    ]

let domain_t =
  let equal (da, ipa) (db, ipb) =
    Domain_name.equal da db
    && opt_eq (fun a b -> match a, b with
        | `Interface a, `Interface b -> String.equal a b
        | `Ip a, `Ip b -> Ipaddr.V4.Prefix.compare a b = 0
        | `Ip_range (sa, ea), `Ip_range (sb, eb) ->
          ipv4_eq sa sb && ipv4_eq ea eb
        | _ -> false) ipa ipb
  in
  Alcotest.testable pp_domain equal

let ok_domain () =
  let input = "home.lan" in
  let expected = Domain_name.of_string_exn "home.lan", None in
  Alcotest.(
    check
      (result domain_t msg_t)
      "Domain is good" (Ok expected)
      (parse_one_arg domain input))

let ok_domain2 () =
  let input = "thekelleys.org.uk,192.168.0.0/24" in
  let expected =
    Domain_name.of_string_exn "thekelleys.org.uk",
    Some (`Ip (Ipaddr.V4.Prefix.of_string_exn "192.168.0.0/24"))
  in
  Alcotest.(
    check
      (result domain_t msg_t)
      "Domain is good" (Ok expected)
      (parse_one_arg domain input))

let ok_domain3 () =
  let input = "thekelleys.org.uk,192.168.0.1,192.168.7.255" in
  let expected =
    Domain_name.of_string_exn "thekelleys.org.uk",
    Some (`Ip_range Ipaddr.V4.(of_string_exn "192.168.0.1", of_string_exn "192.168.7.255"))
  in
  Alcotest.(
    check
      (result domain_t msg_t)
      "Domain is good" (Ok expected)
      (parse_one_arg domain input))

let tests =
  [
    ("DHCP range", `Quick, ok_dhcp_range);
    ("DHCP range with netmask", `Quick, ok_dhcp_range_with_netmask);
    ("DHCP range static", `Quick, ok_dhcp_range_static);
    ("DHCP host thedoctor", `Quick, ok_dhcp_host_thedoctor);
    ("DHCP host tardis", `Quick, ok_dhcp_host_tardis);
    ("DHCP host sonicscrewdriver", `Quick, ok_dhcp_host_sonicscrewdriver);
    ("DHCP host apollon", `Quick, ok_dhcp_host_apollon);
  ]
  @ ok_dhcp_host_dnsmasq_conf_example @
  [
    ("Domain from example", `Quick, ok_domain);
    ("Domain from manpage", `Quick, ok_domain2);
    ("Domain with ip range", `Quick, ok_domain3);
  ]

let string_of_file filename =
  let config_dir = "sample-configuration-files" in
  let file = Filename.concat config_dir filename in
  try
    let fh = open_in file in
    let content = really_input_string fh (in_channel_length fh) in
    close_in_noerr fh;
    content
  with _ -> Alcotest.failf "Error reading file %S" file

let test_configuration config file () =
  match parse_file (string_of_file file) with
  | Error (`Msg msg) -> Alcotest.failf "Error parsing %S: %s" file msg
  | Ok data ->
      Alcotest.(check int)
        "Number of configuration items matches" (List.length config)
        (List.length data)

let config_file_tests =
  [
    ("First example", `Quick, test_configuration [] "simple.conf");
    ( "White space and comments",
      `Quick,
      test_configuration [] "whitespace-and-comments.conf" );
  ]

let tests =
  [ ("Config tests", tests); ("Configuration file tests", config_file_tests) ]

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter ();
  Logs.(set_level @@ Some Debug);
  Alcotest.run "DNSvizor tests" tests
