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

open Digestif
open Command

type hash_type =
  | BLAKE256 (* The standard blake2b 32 *)
  | TWOX128 (* concate two xx hash into one *) 

module BLAKE256 = Make_BLAKE2B (struct let digest_size = 32 end)

let build_hash_for_arg hash node =
  match node with
  | Arg.STR str -> begin
      match hash with
      | BLAKE256 ->
          let h = BLAKE256.digest_string str in
          Arg.STR ("0x" ^ (BLAKE256.to_hex h))
      | TWOX128 ->
          raise (CommandError
            "twox128 not supported yet")
    end
  | _ -> raise (CommandError
        "can not hash non-string node")


