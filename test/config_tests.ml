open Dnsvizor.Config_parser

let opt_eq f a b = match a, b with
  | None, None -> true
  | Some a, Some b -> f a b
  | None, Some _ | Some _, None -> false

let ip_eq a b = Ipaddr.V4.compare a b = 0

let mode_eq a b = match a, b with
  | `Static, `Static | `Proxy, `Proxy -> true
  | `Static, _ | `Proxy, _ -> false

let dhcp_range_t =
  let equal a b =
    ip_eq a.start_addr b.start_addr &&
    opt_eq ip_eq a.end_addr b.end_addr &&
    opt_eq mode_eq a.mode b.mode &&
    opt_eq ip_eq a.netmask b.netmask &&
    opt_eq ip_eq a.broadcast b.broadcast &&
    opt_eq Int.equal a.lease_time b.lease_time
  in
  Alcotest.testable pp_dhcp_range equal

let ok_dhcp_range () =
  let input = "192.168.0.50,192.168.0.150,12h" in
  let expected = { start_addr = Ipaddr.V4.of_string_exn "192.168.0.50" ;
                   end_addr = Some (Ipaddr.V4.of_string_exn "192.168.0.150") ;
                   mode = None ; netmask = None ; broadcast = None ;
                   lease_time = Some (12 * 60 * 60) ;
                 }
  in
  Alcotest.(check (result dhcp_range_t string) "DHCP range is good" (Ok expected)
              (parse_one dhcp_range input))

let tests =
  [
    ("DHCP range 1", `Quick, ok_dhcp_range);
  ]

let tests = [ ("Config tests", tests) ]

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter ();
  Logs.(set_level @@ Some Debug);
  Alcotest.run "DNSvizor tests" tests
