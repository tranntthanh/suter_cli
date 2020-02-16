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

let zkp_client uri cmd_file =

  let open Command in

  let%lwt recv, send = setup_connection uri in
  let%lwt db = init_command_db send recv in
  let%lwt in_channel = match cmd_file with
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
  let file = ref None in
  let set_uri s = uri := s in
  let set_file f = file := Some f in
  let fetch_cmd s = cmd := s in
  let spec_args = Arg.align [
      "-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel";
      "-url", Arg.String set_uri, " ZKP reset sign process";
      "-f", Arg.String set_file, " ZKP reset sign process";
    ] in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse spec_args fetch_cmd usage_msg;
  Lwt_main.run (zkp_client (Uri.of_string !uri) !file)
