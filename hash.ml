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


