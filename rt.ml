(*
 * Copyright 2019 Suterusu project <contact@suterusu.io>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open Lwt.Infix
open Websocket
open Connection
open Command
open Dsl
open Console
open Hash
open Crypto


let load_fun fname args = Lwt.return @@ fun _ -> Lwt.return None

module ExecuteClosure = struct
  type map = Arg.t JSONClosure.t

  type 'a remote_call = Command.t -> ('a option) Lwt.t

  let closure = ref JSONClosure.empty

  let set_var vname var =
    let var = Arg.flat_node !closure var in
    if JSONClosure.mem vname !closure then
      raise @@ CommandError (vname ^ " is not mutable")
    else
      closure := JSONClosure.add vname var !closure

  let display_all_vars =
    ignore @@ JSONClosure.mapi (fun n arg ->
      io_printf "> %s = %s" n (Arg.to_string arg)
    ) !closure

  (*
   * Execute the command ast
   * The remote_exec is usually
   * provied by a wsclient to
   * invoke a remote call.
   *)
  let run db lname cmd remote_call =
    let%lwt lvar = match cmd with
    | NOP -> display_all_vars; Lwt.return None
    | LVAR arg ->
      Lwt.return @@ Some arg
    | HASH (hash, arg) ->
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Some (build_hash_for_arg hash arg)
    | CRYPTO (crypto, arg) ->
      let arg = Arg.flat_node !closure arg in
      Lwt.return @@ Some (build_crypto_public crypto arg)
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
      let%lwt cmd = Lwt.return @@ compose !closure db tname arg "1" in
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

let is_command cmd =
    if (String.length (String.trim cmd) == 0) then false else true

let execute_script_stream stream (recv, send) db cls =
  let rec react_forever env = match db with
    | None -> begin
        let%lwt _ = Lwt_log.debug ~section
        "Exit on error." in
        send @@ Frame.close 1000
      end
    | Some db -> begin
        Lwt_io.(read_line_opt stream) >>= function
        | None -> begin
            let%lwt _ = Lwt_log.debug ~section
	        "Got EOF. Sending a close frame." in
            send @@ Frame.close 1000
          end
        | Some cmd when is_command cmd -> begin
            let%lwt _ = io_printf "%s\n" (String.trim cmd) in
            let%lwt _ = try
                let lvl, cmd = parse_arg @@ Stream.of_string cmd in
                let remote_call = do_ipc send recv in
                ExecuteClosure.run db lvl cmd remote_call
            with
            | Ploc.Exc (loc, e) ->
                let msg = Printexc.to_string e in
                ignore @@ io_printf "> Error: start %d, end %d\n> "
                    (Ploc.first_pos loc) (Ploc.last_pos loc);
                io_printf "Details %s\n> " msg
            | TypingError (t,a) ->
                io_printf "> Command Typing Error: %s is expected to have type %s\n> "
                (Arg.to_string a) (ArgType.to_string t)
            in
            react_forever env
          end
        | _ -> react_forever env
      end
  in react_forever JSONClosure.empty

