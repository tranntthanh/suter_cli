(*
 * Copyright 2019 Suterusu project <contact:suterusu.io>
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

open Stream
open Command
open Lwt.Infix
open Hash
open Crypto
open Digestif
open Closure
open Ast
open Encode

exception DSLError of string

let g_dsl = Grammar.gcreate (Plexer.gmake ())

let arg_exp = Grammar.Entry.create g_dsl "json_arg_exp"
let cmd_exp = Grammar.Entry.create g_dsl "json_cmd_exp"

EXTEND
    GLOBAL: cmd_exp;
    arg_exp: [
        [ "{"; attrs = LIST1 key_value_exp SEP ","; "}"
        -> Arg.ATTR attrs ]
      | [ str = STRING -> Arg.STR str ]
      | [ "`"; char = INT -> Arg.CHAR (Char.chr (int_of_string char)) ]
      | [ int= INT -> Arg.INT (int_of_string int) ]
      | [ var = cname_exp -> Arg.VAR var ]
      | [ "["; args = LIST0 arg_exp SEP ","; "]" -> Arg.ARGS args ]
    ];
    key_value_exp: [
        [ name=cname_exp; ":"; exp=arg_exp
        -> (name, exp)]
    ];
    hash_type: [
        [ "blake256" -> BLAKE256]
      | [ "blake128" -> BLAKE128]
      | [ "twox128" -> TWOX128]
      | [ "twox64" -> TWOX64]
      | [ "plain" -> PLAIN]
    ];
    crypto_type: [
        [ "ed25519" -> ED25519]
    ];
    encode_type: [
        [ "compact" -> COMPACT]
      | [ "u128" -> U128]
    ];
    cmd_exp: [
      [ lvl =vname_exp; ":="; rvl=cmd_rvalue->
            (Some (to_vname lvl), rvl) ]
    | [ rvl=cmd_rvalue ->
            (None, rvl)
      ]
    ];

    cmd_rvalue : [
      [ tname=arg_exp ->
          LVAR tname ]
    | [ "@"; "check";
          vname=vname_exp; ":";
          tname=tname_exp ->
            CHECK (tname, vname)]
    | [ "@"; "crypto"; crypto=crypto_type;
          seed=arg_exp -> CRYPTO (crypto, seed) ]
    | [ "@"; "sign"; crypto=crypto_type;
          arg=arg_exp; "|>"; seed=arg_exp -> SIGN (crypto, seed, arg) ]
    | [ "@"; "hash"; hash=hash_type;
          arg=arg_exp -> HASH (hash, arg) ]
    | [ "@"; "decode"; arg=arg_exp -> DECODE arg ]
    | [ "@"; "encode"; encoder=encode_type;
          arg=arg_exp -> ENCODE (encoder, arg) ]
    | [ "@"; "display"; tname=tname_exp ->
            DISPLAY tname]
    | [ "@"; "send"; node=arg_exp; "|>";
          tname=tname_exp ->
            SEND (tname, node) ]
    | [ "@"; "subscribe" -> SUBSCRIBE ]
    | [ "@"; "as_hex"; node=arg_exp -> AS_HEX node]
    | [ "@"; "send"; node=arg_exp; "|>";
          tname=tname_exp ->
            SEND (tname, node) ]
    | [ "@"; "call"; fname=cname_exp;
          args = LIST0 arg_exp SEP " " ->
            CALL (fname, args) ]
    ];

    tname_exp: [[ n = LIDENT -> to_tname n]];
    vname_exp: [[ n = LIDENT -> to_vname n]];
    cname_exp: [[ n = LIDENT -> to_cname n]];
END

let parse_arg t = Grammar.Entry.parse cmd_exp t
