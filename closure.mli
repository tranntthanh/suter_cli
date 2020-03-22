open Lwt.Infix
open Websocket
open Connection
open Command
open Ast

module ExecuteClosure : sig

type 'a remote_call = Command.t -> ('a option) Lwt.t
type 'a subscribe = unit -> ('a option) Lwt.t

val run : ArgType.t CommandDB.TypeDb.t
    -> JSONClosure.key option -> ast -> Arg.t remote_call -> Arg.t subscribe
    -> unit Lwt.t

val set_var : string -> Arg.t -> unit
val flat_str : Arg.t -> string
val display_all_vars : unit -> unit
end

