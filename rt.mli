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
