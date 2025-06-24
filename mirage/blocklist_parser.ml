open Angstrom

module Log =
  (val Logs.(src_log (Src.create ~doc:"Blocklist parser" "blocklist-parser"))
     : Logs.LOG)

let skippable_ws =
  skip_while (function
      | ' ' | '\t' -> true
      | _ -> false)

let skippable_ws1 =
  skip (function
       | ' ' | '\t' -> true
       | _ -> false) *>
  skippable_ws

let a_ipv4_dotted_quad =
  take_while1 (function '0' .. '9' | '.' -> true | _ -> false) >>= fun ip ->
  match Ipaddr.V4.of_string ip with
  | Error (`Msg x) -> fail (Fmt.str "Invalid IPv4: %s: %S" x ip)
  | Ok ip -> return ip

let a_ipv6_coloned_hex =
  take_while1 (function
    | '0' .. '9' | ':' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false)
  >>= fun ip ->
  match Ipaddr.V6.of_string ip with
  | Error (`Msg x) -> fail (Fmt.str "Invalid IPv6: %s: %S" x ip)
  | Ok ip -> return ip

let a_ip =
  a_ipv4_dotted_quad
  >>| (fun v4 -> Ipaddr.V4 v4)
  <|> (a_ipv6_coloned_hex >>| fun v6 -> Ipaddr.V6 v6)

let hostname source =
  let* str =
    take_till (function
        | '\x00' .. '\x1f' | ' ' | '#' -> true
        | _ -> false)
  in
  if String.length str = 0 then
    fail "zero hostname"
  else
    match Result.bind (Domain_name.of_string str) Domain_name.host with
    | Error `Msg e ->
      Log.warn (fun m -> m "%s: Invalid domain name %s: %S" source e str);
      return None
    | Ok hostname -> return (Some hostname)

let opt_cons x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

let many1_opt p =
  lift2 opt_cons p
    (fix (fun m -> lift2 opt_cons p m <|> return []))

let host source =
  let* start_pos = pos in
  let* ip = a_ip in
  let* end_pos = pos in
  let* hostnames = many1_opt (skippable_ws1 *> hostname source) in
  let () =
    match ip with
    | Ipaddr.V4 v4 ->
      Log.info (fun m ->
          if Ipaddr.V4.(compare any) v4 <> 0 then
            (* TODO: we don't know the source /o\ *)
            m "%s: Non 0.0.0.0 ip address at byte offset %u-%u: %a"
              source start_pos end_pos Ipaddr.V4.pp v4)
    | Ipaddr.V6 v6 ->
      Log.info (fun m ->
          if Ipaddr.V6.(compare unspecified) v6 <> 0 then
            m "%s: Non 0.0.0.0 ip address at byte offset %u-%u: %a"
              source start_pos end_pos Ipaddr.V6.pp v6)
  in
  return hostnames

let comment =
  char '#' *>
  skip_while (function '\r' | '\n' -> false | _ -> true)

let skip_garbage source =
  let* start_pos = pos in
  let* garbage = consumed (skip_while (function '\r' | '\n' -> false | _ -> true)) in
  let* end_pos = pos in
  if start_pos = end_pos then
    fail "no garbage to skip"
  else
    (Log.warn (fun m -> m "%s: Skipped garbage at byte offset %u-%u: %S" source start_pos end_pos garbage);
     return ())

let empty_line =
  peek_char >>= function
  | None | Some ('\r' | '\n') -> return ()
  | _ -> fail "not an empty line"

let line source =
  skippable_ws *>
  let* pos = pos in
  choice ~failure_msg:"This shouldn't happen" [
    empty_line *> return [];
    (host source <?> "host line");
    (comment <?> "comment") *> return [];
    (skip_garbage source <?> "skip garbage") *> return []
  ] <* skippable_ws <* (option () comment) <*
  (end_of_line <|> end_of_input)

let lines source serial =
  let soa = Blocklist.soa source serial in
  let rec loop acc =
    (end_of_input *> return acc) <|>
    let* hosts = line source <?> "line" <* commit in
    let acc =
      List.fold_left (fun acc host ->
          Dns_trie.insert host Dns.Rr_map.Soa soa acc)
        acc hosts
    in
    loop acc
  in
  loop
