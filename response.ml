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

(*
 * There is no good JDT implementation so far
 * We uses camlp5 to construct a dynamic parser based
 * on the Command Description File.
 *
 * In substrate the Commands description file is
 * encoded in the metadata.
 *)

module ResponseDecoder = struct
  open Hex

  (*
   * After we decoded the meta from hex to string
   * we still need codec to decode the remaining stuff.
   *
   * This is not good, which means the encoding of
   * substrate rpc methods is binded with a non-standard
   * encoder.
   *)

  let decode_hex_string hex_str =
    let s = String.sub hex_str 2
        (String.length hex_str - 2) in
    Hex.to_string @@ `Hex s

  exception InvalidResponse of string

  (* We assume that the json result is always encoded *)
  let get_response str =
      let open Yojson.Basic in
      let json = from_string str in
      let result = try
        json |> Util.member "result"
      with
      | Not_found -> begin
        try
          let error = json |> Util.member "error" in
          raise @@ InvalidResponse (Yojson.Basic.to_string error)
        with _ ->
          raise @@ InvalidResponse ("result "^ str ^ " does not exist, maybe an error occurred.")
        end
      in
      Command.Arg.of_json result
end

