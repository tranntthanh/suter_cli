(* open Cfg *)
Printexc.record_backtrace true;;
open Codeflow

module TrivialBlock = struct
  type elt = string
  type t = {
    statements: elt list;
    mutable next: ((t ref) list);
    index: int;
    id: string;
  }
  let index b = b.index
  let id b = b.id
  let compare a b = a.index - b.index
  let equal b a = a.index = b.index
  let elements b = b.statements
  let next b = List.map (fun b -> !b) b.next
  let make idx name sts = {
    statements = sts;
    next = [];
    index = idx;
    id = name;
  }
  let connect a b = a.next <- b :: (a.next)
end

module TrivialCFG = Cfg.Make(TrivialBlock)
;;

let translator b: string TrivialCFG.Statement.t =
  TrivialCFG.Statement.mkComment (TrivialBlock.id b)
;;

let merge_to_string = function 
| TrivialCFG.Merge bgg -> TrivialCFG.BlockClosure.id bgg
| _ -> "Diverge"
;;


print_endline "test 1:";

let tA = TrivialBlock.make 1 "A" [] in

let bset = TrivialCFG.BlockSet.singleton tA in

ignore @@ TrivialCFG.aggregate tA bset false;

print_endline "\ntest 2:";

let tB = TrivialBlock.make 2 "B" [] in

TrivialBlock.connect tA (ref tB);

ignore @@ TrivialCFG.aggregate tA (TrivialCFG.BlockSet.add tB bset) false;

print_endline "\ntest 3:";

TrivialBlock.connect tB (ref tA);

ignore @@ TrivialCFG.aggregate tA (TrivialCFG.BlockSet.add tB bset) false;

print_endline "\ntest 4:";

let tC = TrivialBlock.make 3 "C" [] in
TrivialBlock.connect tB (ref tC);
let bset = TrivialCFG.BlockSet.add tB bset in
let bset = TrivialCFG.BlockSet.add tC bset in

ignore @@ TrivialCFG.aggregate tA bset false;

print_endline "\ntest 5:";

let tD = TrivialBlock.make 4 "D" [] in
TrivialBlock.connect tC (ref tD);
let bset = TrivialCFG.BlockSet.add tD bset in

let agg = TrivialCFG.aggregate tA bset false in

let bgg = TrivialCFG.get_merge_point agg agg in
print_endline @@ "\nTest merge point:" ^ (TrivialCFG.BlockClosure.id agg)
     ^ " --merge-> " ^ merge_to_string bgg;

TrivialCFG.debug agg;

print_endline "\ntest 6:";

let tE = TrivialBlock.make 5 "E" [] in
TrivialBlock.connect tA (ref tE);
let bset = TrivialCFG.BlockSet.add tE bset in

let agg = TrivialCFG.aggregate tA bset false in

let bgg = TrivialCFG.get_merge_point agg agg in
print_endline @@ "\nTest merge point:" ^ (TrivialCFG.BlockClosure.id agg)
     ^ " --merge-> " ^ merge_to_string bgg;

TrivialCFG.debug agg;

print_endline "testing trace:";

let statement () =
    let st = TrivialCFG.trace agg (TrivialCFG.Statement.mkFallThrough ())
    agg None tA translator in
    let emitter = Codeflow.Emitter.mkEmitter () in
    TrivialCFG.Statement.emit emitter (snd st)

in statement ();
