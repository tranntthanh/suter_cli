open Console
open Hash
open Crypto
open Ast
open Command
open Lwt.Infix

let load_fun fname args = Lwt.return @@ fun _ -> Lwt.return None

module ExecuteClosure = struct
  type map = Arg.t JSONClosure.t

  type 'a remote_call = Command.t -> ('a option) Lwt.t
  type 'a subscribe = unit -> ('a option) Lwt.t

  let closure = ref JSONClosure.empty

  let set_var vname var =
    let var = Arg.flat_node !closure var in
    if JSONClosure.mem vname !closure then
      raise @@ CommandError (vname ^ " is not mutable")
    else
      closure := JSONClosure.add vname var !closure

  let flat_str node = Arg.flat_str @@ Arg.flat_node !closure node

  let display_all_vars _  =
    ignore @@ JSONClosure.mapi (fun n arg ->
      io_printf "> %s = %s" n (Arg.to_string arg)
    ) !closure

  (*
   * Execute the command ast
   * The remote_exec is usually
   * provied by a wsclient to
   * invoke a remote call.
   *)
  let run db lname cmd remote_call subscribe =
    let%lwt lvar = match cmd with
    | NOP -> display_all_vars (); Lwt.return None
    | LVAR arg ->
      Lwt.return @@ Some arg
    | HASH (hash, arg) ->
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Some (build_hash_for_arg hash arg)
    | CRYPTO (crypto, arg) ->
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Some (build_crypto_public crypto arg)
    | DECODE (arg) ->
      let arg = Arg.flat_node !closure arg in
      let arg = Arg.flat_hex arg in
      let arg_str = Hex.to_string (`Hex arg) in
      Lwt.return @@ Some (Arg.STR arg_str)
    | ENCODE (arg) ->
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Encode.encode_compact_hex arg
    | AS_HEX (arg) ->
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Some (Arg.STR ("0x" ^ Arg.flat_str arg))
    | SIGN (crypto, seed, arg) ->
      let seed = Arg.flat_node !closure seed in
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Some (build_crypto_sign crypto seed arg)
    | DISPLAY n ->
      let v = JSONClosure.find n !closure in
      io_printf "> %s = %s\n" n (Arg.to_string v) >>= fun _ ->
      Lwt.return None
    | CHECK (tname, vname) ->
      let arg = Arg.VAR vname in
      let cmd = compose !closure db tname arg "0" in
      io_printf "> %s\n" (Command.to_string cmd) >>= fun _ ->
      Lwt.return None
    | SEND (tname, arg) -> begin
      let%lwt cmd = Lwt.return @@ compose !closure db tname arg "3" in
      let%lwt result = remote_call cmd in
      match result with
      | Some r ->
          io_printf "> Response: %s\n"
              (Arg.to_string r) >>= fun _ ->
          Lwt.return (Some r)
      | None ->
          io_printf "> Command Error, No Response\n" >>= fun _ ->
          Lwt.return None
      end
    | SUBSCRIBE -> begin
      let rec scb () =
        io_printf "> Waiting for subscription:\n" >>= fun _ ->
        let%lwt result = subscribe () in
        match result with
        | Some r ->
          io_printf "> Response: %s\n"
              (Arg.to_string r) >>= fun _ ->
          Lwt.return None
        | None ->
          io_printf "> Command Error, No Response\n" >>= fun _ ->
          Lwt.return None
      in scb ()
      end
    | CALL (fname, args) -> begin
          (*
           * every fun has an hash map to map args into (key,arg) map
           * and execute the fun with this map
           *)
          let%lwt f = load_fun fname args in
          f ()
      end
    in
    io_printf "> " >>= fun _ ->
    match lvar, lname with
    | Some v, Some n -> set_var n v; Lwt.return ()
    | _ , _ -> Lwt.return ()
end


