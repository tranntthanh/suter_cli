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

module Hash64 = XXHash.XXH64

type hash_type =
  | BLAKE256 (* The standard blake2b 32 *)
  | TWOX64 (* xx hash *)
  | TWOX128 (* concate two xx hash into one *)
  | PLAIN (* plat the json into a plain hex *)

module BLAKE256 = Make_BLAKE2B (struct let digest_size = 32 end)

let int64_to_bytes hash =
    let w n i =
        let x = Int64.unsigned_rem (Int64.shift_right_logical i (8 * n)) 256L in
        Char.chr ((Int64.to_int x) mod 256)
    in Bytes.init 8 (fun n -> w n hash)

let hash64_with_seed seed str =
    let s = Hash64.create () in
    Hash64.reset ?seed:(Some seed) s;
    Hash64.update s str;
    let hash = Hash64.digest s in
    let bytes = int64_to_bytes hash in
    Hash64.free s;
    Hex.show @@ Hex.of_string (Bytes.to_string bytes)

let hash64 str = hash64_with_seed (Int64.of_int 0) str

let hash128 str =
    let s1 = hash64_with_seed (Int64.of_int 0) str in
    let s2 = hash64_with_seed (Int64.of_int 1) str in
    (* Little end *)
    s1 ^ s2

let build_hash_for_str hash str =
  match hash with
  | BLAKE256 ->
      let h = BLAKE256.digest_string str in
      Arg.STR ("0x" ^ (BLAKE256.to_hex h))
  | TWOX128 ->
      Arg.STR ("0x" ^ (hash128 str))
  | TWOX64 ->
      Arg.STR ("0x" ^ (hash64 str))
  | PLAIN -> Arg.STR ("0x" ^ str)


let build_hash_for_arg hash node =
  let hash_str = Arg.flat_hex node in
  build_hash_for_str hash hash_str
