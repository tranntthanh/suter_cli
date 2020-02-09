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

module ExecuteClosure : sig

type 'a remote_call = Command.t -> ('a option) Lwt.t

val run : ArgType.t CommandDB.TypeDb.t
    -> JSONClosure.key option -> ast -> Arg.t remote_call
    -> unit Lwt.t
end

val execute_script_stream : in_channel
    -> ((unit -> Websocket.Frame.t Lwt.t) * (Websocket.Frame.t -> unit Lwt.t))
    -> (ArgType.t CommandDB.TypeDb.t) option
    -> Command.t JSONClosure.t
    -> unit Lwt.t


