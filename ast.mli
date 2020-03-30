open Command
open Hash
open Crypto
open Encode

type vname = string
type tname = string

type ast =
  | NOP
  | LVAR of Arg.t
  | CHECK of tname * vname
  | HASH of (hash_type * Arg.t)
  | CRYPTO of (crypto_type * Arg.t)
  | DECODE of (Arg.t)
  | ENCODE of (encode_type * Arg.t)
  | AS_HEX of (Arg.t)
  | SIGN of (crypto_type * Arg.t * Arg.t)
  | DISPLAY of tname
  | SEND of tname * Arg.t
  | SUBSCRIBE
  | CALL of cname * (Arg.t list)

val to_cname: string -> cname
val to_tname: string -> tname
val to_vname: string -> vname
