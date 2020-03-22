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

module JSONClosure = Map.Make(String)

let add_var vname c closure = JSONClosure.add vname c closure

module TypesDb = Map.Make(String)

(* The error type *)
exception CommandError of string
exception DecodeArgError of string

type cname = string

module Arg = struct

  type t =
    | NULL
    | STR of string
    | CHAR of char
    | INT of int
    | BOOL of bool
    | VAR of string
    | ARGS of t list
    | ATTR of (string * t) list

  let rec to_string t = match t with
  | NULL -> "null"
  | STR s -> "\"" ^ s ^ "\""
  | INT i -> Int.to_string i
  | BOOL b -> if b then "true" else "false"
  | VAR s -> s
  | CHAR c -> "`" ^ Int.to_string (Char.code c)
  | ARGS args -> begin
      match args with
      | [] -> "[]"
      | hd :: ls ->
          let c = List.fold_left (fun acc c ->
            acc ^ ", " ^ (to_string c)
          ) (to_string hd) ls in
          "[ " ^ c ^ " ]"
    end
  | ATTR attrs -> begin
      let tos (n, t) = n ^ ":" ^ (to_string t) in
      match attrs with
      | [] -> "{}"
      | hd :: ls -> let c = List.fold_left (fun acc c ->
            acc ^ ", " ^ (tos c)
          ) (tos hd) ls in
          "[ " ^ c ^ " ]"
    end

  let map f t = match t with
  | ARGS args -> ARGS (List.map f args)
  | ATTR attrs -> ATTR (List.map (fun (c, t) -> (c, f t)) attrs)
  | c -> f c

  let flat_node env t =
    let rec flat t = match t with
    | ATTR attrs -> map flat t
    | ARGS args -> map flat t
    | VAR vname -> flat @@ JSONClosure.find vname env
    | c -> c
    in flat t

  let rec of_json (t:Yojson.Basic.t) = match t with
  | `String s -> STR s
  | `Int i -> INT i
  | `Bool b -> BOOL b
  | `List ls -> ARGS (List.map (fun c -> of_json c) ls)
  | `Assoc ls -> ATTR (List.map
        (fun (n,c) -> (n, of_json c)) ls)
  | `Null -> NULL
  | _ -> raise @@ DecodeArgError (Yojson.Basic.to_string t)

  let rec flat_hex node =
    let starts_with_0x str = try
      String.sub str 0 2 = "0x"
    with _ -> false in
    match node with
    | STR str -> if (starts_with_0x str)
        then (String.sub str 2 (String.length str - 2))
        else str
    | CHAR c ->
        let s = Bytes.to_string @@ Bytes.init 1 (fun n -> c) in
        (match Hex.of_string s with | `Hex hexstr -> hexstr)
    | ARGS ls -> List.fold_left (fun acc c -> acc ^ flat_hex c) "" ls
    | _ -> raise @@ CommandError ("Can not flat " ^ to_string node ^ " to string")

  let rec flat_str node =
    match node with
    | STR str -> str
    | ARGS ls -> List.fold_left (fun acc c -> acc ^ flat_str c) "" ls
    | _ -> raise @@ CommandError ("Can not flat " ^ to_string node ^ " to string")

end

module ArgType = struct

  type t =
    | TString
    | TInt
    | TBool
    | TList of t list
    | TJson of (string * t) list

  let rec to_string t = match t with
  | TString -> "STRING"
  | TInt -> "INT"
  | TBool -> "BOOL"
  | TList argtyps -> begin
      match argtyps with
      | [] -> "[]"
      | hd :: ls -> let c = List.fold_left (fun acc c ->
            acc ^ ", " ^ (to_string c)
          ) (to_string hd) ls in
          "[ " ^ c ^ " ]"
    end
  | TJson attrs -> begin
      let tos (n, t) = n ^ ":" ^ (to_string t) in
      match attrs with
      | [] -> "{}"
      | hd :: ls ->
          let c = List.fold_left (fun acc c ->
            acc ^ ", " ^ (tos c)
          ) (tos hd) ls in
          "{ " ^ c ^ " }"
    end


end

exception TypingError of (ArgType.t * Arg.t)

module Command = struct
  open Arg
  open ArgType
  module Json = Yojson.Basic

  type t = Json.t

  let to_string = Json.to_string

  (* Recursively type check the command *)
  let rec type_check t node: Json.t
    = match t,node with
    | TString, STR str -> `String str
    | TInt, INT i -> `Int i
    | TBool, BOOL b -> `Bool b
    | TList ts, ARGS args -> begin
        `List (List.map2 (fun t a -> type_check t a)
            ts args)
      end
    | TJson ts, ATTR args -> begin
        `Assoc (List.map2 (fun t a ->
            match t, a with
            | (n,t), (n', arg) when n = n' ->
                (n, type_check t arg)
            | (_,t), (_, arg) ->
                raise (TypingError (t, arg))
        ) ts args)
      end
    | _ -> raise (TypingError (t, node))

  (* Read Args and Construct a json node *)
  let rec compose_cmd_node env ctx typ arg =
    let open Json in
    match arg with
      | VAR vname -> type_check typ @@ JSONClosure.find vname env
      | _ -> type_check typ arg


end


module CommandDB = struct
  (* TODO change the following into functor *)
  module Out = Lwt_io

  type t = Yojson.Basic.t

  module TypeDb = Map.Make(String)

  type db = ArgType.t TypeDb.t

  let to_string t = Yojson.Basic.to_string t

  (*
   * TODO not implemented yet
   *)
  let compile db cmdjson = cmdjson

  let query_command env db cname args =
    let cmd_type = try
        TypeDb.find cname db
    with Not_found ->
        raise (CommandError ("command " ^ cname
        ^ " not found!"))
    in
    Command.compose_cmd_node env db cmd_type args

  (*
   * TODO not implemented yet
   *)
  let create t = t
end

let compose env db method_name arg cid =
  let open CommandDB in
  let open Command in
  try
    let arg = query_command env db method_name arg in
    `Assoc [
          ("jsonrpc", `String "2.0")
        ; ("method", `String method_name)
        ; ("params", arg)
        ; ("id", `String cid)
    ]
  with
  | Not_found -> raise @@ CommandError method_name
  | e -> raise e

let compose_unsafe method_name args id =
    let cmd = [
          ("jsonrpc", `String "2.0")
        ; ("method", `String method_name)
        ; ("id", `Int id)
    ] in
    match args with
    | None -> `Assoc cmd
    | Some args -> `Assoc (cmd @ ["para", args])

