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

Printexc.record_backtrace true;;

open Lwt.Infix
open Websocket
open Connection
open Config
open Dsl
open Console

let init_command_db send recv : 'a option Lwt.t=
  let open Command in
  (* let m = "state_getMetadata" in *)
  (* let cmd = compose_unsafe m None 1 in *)

  let db = CommandDB.TypeDb.empty in
  (* let%lwt result = do_ipc send recv cmd in *)
  let%lwt _ = io_printf "> entering script mode:\n> " in
  (* TODO construct the formal db here *)
  Lwt.return @@ Some (dft_cdb db)

type script_info = {
  file: string option;
  args: string list;
}

let zkp_client uri cmd_file =

  let open Command in

  ignore @@ List.fold_left (fun c value ->
    let name = "arg" ^ string_of_int c in
    Closure.ExecuteClosure.set_var name (Arg.STR value);
    c + 1
  ) 0 cmd_file.args;

  let%lwt recv, send = setup_connection uri in
  let%lwt db = init_command_db send recv in
  let%lwt in_channel = match cmd_file.file with
    | Some f -> Lwt_io.open_file Lwt_io.input f
    | _ -> Lwt.return Lwt_io.stdin in
  Rt.execute_script_stream in_channel (recv,send) db JSONClosure.empty

let apply_loglevel = function
| 2 -> Lwt_log.(add_rule "*" Info)
| 3 -> Lwt_log.(add_rule "*" Debug)
| _ -> ()

let () =
  let uri = ref "" in
  let cmd = ref "" in
  let file = ref {file=None; args=[]} in
  let set_uri s = uri := s in
  let set_file f = file := {!file with file = Some f} in
  let push_arg arg = file := {!file with args = !file.args @ [arg]} in
  let fetch_cmd s = cmd := s in
  let spec_args = Arg.align [
      "-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel";
      "-url", Arg.String set_uri, " ZKP reset sign process";
      "-f", Arg.String set_file, " suter script files";
      "-args", Arg.Rest push_arg, " script args";
    ] in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse spec_args fetch_cmd usage_msg;
  Lwt_main.run (zkp_client (Uri.of_string !uri) !file)
