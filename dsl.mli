open Stream
open Command

type vname = string
type tname = string

type ast =
  | NOP
  | LVAR of Arg.t
  | CHECK of tname * vname
  | DISPLAY of tname
  | SEND of tname * Arg.t

val parse_arg : char Stream.t -> vname option * ast

