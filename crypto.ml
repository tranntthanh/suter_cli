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

open Command
open Tweetnacl

type crypto_type = ED25519
type crypto_method = SIGN | PUBLIC_KEY

let get_the_str_list node n = match node with
  | Arg.ARGS ts ->
    if (List.length ts != n) then
        raise (CommandError ("expect string list of length " ^ string_of_int n))
    else
      List.map (fun c -> match c with
        | Arg.STR str -> str
        | _ -> raise (CommandError "expect string list")
      ) ts
  | _ -> raise (CommandError "expect string list")

let build_crypto_public crypto node =
  match node with
  | Arg.STR seed -> begin
    match crypto with
    | ED25519 -> begin
      let pk, _ = Sign.keypair ~seed:(Hex.to_bigstring (`Hex seed)) () in
      match Hex.of_bigstring (Sign.(to_bytes (public pk))) with
      | `Hex str -> Arg.STR str
      end
    end
  | _ -> raise (CommandError "can not get public key for non-string node")

let build_crypto_sign crypto node arg =
  let msg = Bigstring.of_string @@ Arg.flat_hex arg in
  let msg = Hex.to_bigstring (`Hex (Bigstring.to_string msg)) in
  match node with
    | Arg.STR seed -> begin
      match crypto with
      | ED25519 -> begin
        let _, sk = Sign.keypair ~seed:(Hex.to_bigstring (`Hex seed)) () in
        match Hex.of_bigstring Sign.(detached_extended (extended sk) msg) with
        | `Hex str -> Arg.STR str
        end
      end
    | _ -> raise (CommandError "can not sign for non-string node")
