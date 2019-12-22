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
      try
        let result = json |> Util.member "result" in
        Command.Arg.of_json result
      with Not_found -> begin
        try
          let error = json |> Util.member "error" in
          raise @@ InvalidResponse (Yojson.Basic.to_string error)
        with _ -> 
          raise @@ InvalidResponse "result is a not string"
      end
end

