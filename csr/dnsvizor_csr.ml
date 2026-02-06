let prefix = "csr\000"
let prefix_len = String.length prefix

let encode csr =
  let der = X509.Signing_request.encode_der csr in
  let b = Bytes.create (String.length prefix + 2 + String.length der) in
  Bytes.blit_string prefix 0 b 0 prefix_len;
  Bytes.set_uint16_be b prefix_len (String.length der);
  Bytes.blit_string der 0 b (prefix_len + 2) (String.length der);
  Bytes.unsafe_to_string b

let decode s =
  let ( let* ) = Result.bind in
  let* len =
    if String.length s < prefix_len then Error (`Msg "Truncated")
    else if String.starts_with ~prefix s then
      Ok (String.get_uint16_be s prefix_len)
    else Error (`Msg "Not a CSR")
  in
  let off = prefix_len + 2 in
  let* data =
    (* TODO(reynir): we don't do anything with slack at the end; revise *)
    if String.length s < off + len then Error (`Msg "Truncated")
    else Ok (String.sub s off len)
  in
  X509.Signing_request.decode_der data
