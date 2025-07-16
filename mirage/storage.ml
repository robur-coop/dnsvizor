let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Make (BLOCK : Mirage_block.S) = struct
  module Stored_data = OneFFS.Make (BLOCK)
  open Lwt.Infix

  type t = { disk : Stored_data.t; mutable configuration : string option }

  let write_data t =
    Stored_data.write t.disk (Option.value ~default:"" t.configuration)

  let read_data disk =
    Stored_data.read disk >|= function
    | Ok (Some s) -> Ok (Some s)
    | Ok None -> Ok None
    | Error e ->
        error_msgf "error while reading storage: %a" Stored_data.pp_error e

  let connect block =
    Stored_data.connect block >>= fun disk ->
    read_data disk >|= function
    | Error _ as e -> e
    | Ok configuration -> Ok { disk; configuration }

  let update_configuration t configuration =
    let t' = { t with configuration } in
    write_data t' >|= function
    | Ok () ->
        t.configuration <- configuration;

        Ok ()
    | Error we ->
        error_msgf "error while writing storage: %a" Stored_data.pp_write_error
          we
end
