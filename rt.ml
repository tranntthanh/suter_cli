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


let load_fun fname args = Lwt.return @@ fun _ -> Lwt.return None

module ExecuteClosure = struct
    type map = Arg.t JSONClosure.t

    type 'a remote_call = Command.t -> ('a option) Lwt.t

    let closure = ref JSONClosure.empty

    let set_var vname var =
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
        io_printf ">" >>= fun _ ->
        Lwt.return @@ Some arg
      | DISPLAY n ->
        let v = JSONClosure.find n !closure in
        io_printf "> %s = %s\n>" n (Arg.to_string v) >>= fun _ ->
        Lwt.return None
      | CHECK (tname, vname) ->
        let arg = Arg.VAR vname in
        let cmd = compose !closure db tname arg "0" in
        io_printf "> %s\n> " (Command.to_string cmd) >>= fun _ ->
        Lwt.return None
      | SEND (tname, arg) -> begin
        let%lwt cmd = Lwt.return @@ compose !closure db tname arg "1" in
        let%lwt result = remote_call cmd in
        match result with
        | Some r ->
            io_printf "> Response: %s\n> " 
                (Arg.to_string r) >>= fun _ ->
            Lwt.return (Some r)
        | None ->
            io_printf "> Command Error, No Response\n> " >>= fun _ ->
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
      in match lvar, lname with
      | Some v, Some n -> set_var n v; Lwt.return ()
      | _ , _ -> Lwt.return ()
end
