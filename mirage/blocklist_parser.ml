open Angstrom

module Log =
  (val Logs.(src_log (Src.create ~doc:"Blocklist parser" "blocklist-parser"))
      : Logs.LOG)

let skippable_ws = skip_while (function ' ' | '\t' -> true | _ -> false)

let skippable_ws1 =
  skip (function ' ' | '\t' -> true | _ -> false) *> skippable_ws

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

let localhost = Domain_name.of_string_exn "localhost"

let hostname source =
  let* str =
    take_till (function '\x00' .. '\x1f' | ' ' | '#' -> true | _ -> false)
  in
  if String.length str = 0 then fail "zero hostname"
  else
    match Result.bind (Domain_name.of_string str) Domain_name.host with
    | Error (`Msg e) ->
        Log.warn (fun m -> m "%s: Invalid domain name %s: %S" source e str);
        return None
    | Ok hostname ->
        (* See https://datatracker.ietf.org/doc/html/draft-ietf-dnsop-let-localhost-be-localhost *)
        if Domain_name.is_subdomain ~subdomain:hostname ~domain:localhost then
          return None
        else return (Some hostname)

let opt_cons x xs = match x with None -> xs | Some x -> x :: xs
let sep_by1_opt s p = fix (fun m -> lift2 opt_cons p (s *> m <|> return []))

let host source =
  option () (a_ip *> skippable_ws1)
  *> sep_by1_opt skippable_ws1 (hostname source)

let comment =
  char '#' *> skip_while (function '\r' | '\n' -> false | _ -> true)

let skip_garbage source =
  let* start_pos = pos in
  let* garbage =
    consumed (skip_while (function '\r' | '\n' -> false | _ -> true))
  in
  let* end_pos = pos in
  if start_pos = end_pos then fail "no garbage to skip"
  else (
    Log.warn (fun m ->
        m "%s: Skipped garbage at byte offset %u-%u: %S" source start_pos
          end_pos garbage);
    return ())

let empty_line =
  peek_char >>= function
  | None | Some ('\r' | '\n') -> return ()
  | _ -> fail "not an empty line"

let line source =
  skippable_ws
  *>
  let* pos = pos in
  choice ~failure_msg:"This shouldn't happen"
    [
      empty_line *> return [];
      host source <?> "host line";
      (comment <?> "comment") *> return [];
      (skip_garbage source <?> "skip garbage") *> return [];
    ]
  <* skippable_ws <* option () comment
  <* (end_of_line <|> end_of_input)

let lines source serial =
  let soa = Blocklist.soa source serial in
  let rec loop acc =
    end_of_input *> return acc
    <|>
    let* hosts = line source <?> "line" <* commit in
    let acc =
      List.fold_left
        (fun acc host -> Dns_trie.insert host Dns.Rr_map.Soa soa acc)
        acc hosts
    in
    loop acc
  in
  loop
