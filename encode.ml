open Command
open Stdint
open Console

let u128_to_bytes u =
    let rec encode v =
      if v = (Uint128.of_int 0) then Bytes.empty
      else
        let x = Uint128.rem v (Uint128.of_int 256) in
        let v = Uint128.shift_right_logical v 8 in
        let b = Bytes.init 1 (fun _ -> Char.chr ((Uint128.to_int x) mod 256)) in
        let bs = encode v in
        Bytes.cat b bs
    in
    encode u

let encode_compact u =
  let remain = Uint128.shift_right_logical u 6 in
  let least = (Uint128.to_int (Uint128.shift_left u 2)) mod 256 in
  io_printf "> encoding %s, remaining is %s, least is %d\n> "
        (string_of_int (Uint128.to_int u))
        (string_of_int (Uint128.to_int remain))
        least
  ;
  let bytes =
    if remain = (Uint128.of_int 0) then Bytes.init 1 (fun _ -> Char.chr least)
    else begin
      let bytes = u128_to_bytes remain in
      match Bytes.length bytes with
      | 1 -> Bytes.cat (Bytes.init 1 (fun _ -> Char.chr (least + 1)))
        (Bytes.cat bytes Bytes.empty)
      | 2 -> Bytes.cat (Bytes.init 1 (fun _ -> Char.chr (least + 1)))
        Bytes.empty
      | 3 -> Bytes.cat (Bytes.init 1 (fun _ -> Char.chr (least + 2)))
        (Bytes.cat bytes Bytes.empty)
      | 4 -> Bytes.cat (Bytes.init 1 (fun _ -> Char.chr (least + 2)))
        Bytes.empty
      | n -> begin
        let bytes = u128_to_bytes u in
        let least = (Uint128.to_int (Uint128.shift_left (Uint128.of_int n) 2)) mod 256 in
        let head = least + 3 in
        Bytes.cat (Bytes.init 1 (fun _ -> Char.chr head)) bytes
        end
    end
  in match Hex.of_string (Bytes.to_string bytes) with
  | `Hex hexstr -> hexstr

let encode_compact_hex v =
  match v with
  | Arg.INT i ->
    let hex = encode_compact (Uint128.of_int i) in
    Some (Arg.STR hex)
  | Arg.STR str ->
    (* Only support uint for now *)
    let hex = try
        if (String.sub str 0 2 = "0x") then
          let i = int_of_string str in
          encode_compact (Uint128.of_int i)
        else raise Not_found
      with _ ->
        let i = String.length str in
        io_printf "> encoding length is %d\n> " i;
        encode_compact (Uint128.of_int (i / 2)) ^ str
      in
    Some (Arg.STR hex)
  | _ -> raise @@ CommandError ("Encode Compact Error: not an integer")
