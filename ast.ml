open Command
open Hash
open Crypto

type vname = string
type tname = string

type ast =
  | NOP
  | LVAR of Arg.t
  | CHECK of tname * vname
  | HASH of (hash_type * Arg.t)
  | CRYPTO of (crypto_type * Arg.t)
  | DECODE of Arg.t
  | ENCODE of Arg.t
  | AS_HEX of Arg.t
  | SIGN of (crypto_type * Arg.t * Arg.t)
  | DISPLAY of tname
  | SEND of tname * Arg.t
  | SUBSCRIBE
  | CALL of cname * (Arg.t list)

let to_tname t: tname = t
let to_vname t: vname = t
let to_cname t: cname = t
