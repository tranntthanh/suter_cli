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
open Closure

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
                let subscribe = wait_subscription send recv in
                ExecuteClosure.run db lvl cmd remote_call subscribe
            with
            | Ploc.Exc (loc, e) ->
                let msg = Printexc.to_string e in
                ignore @@ io_printf "> Error: start %d, end %d\n> "
                    (Ploc.first_pos loc) (Ploc.last_pos loc);
                io_printf "Details %s\n> " msg
            | TypingError (t,a) ->
                io_printf "> Command Typing Error: %s is expected to have type %s\n> "
                (Arg.to_string a) (ArgType.to_string t)
            | e -> ignore @@ io_printf "> Unexpected exception!!! dump all vars:\n";
                ExecuteClosure.display_all_vars (); raise e
            in
            react_forever env
          end
        | _ -> react_forever env
      end
  in react_forever JSONClosure.empty

