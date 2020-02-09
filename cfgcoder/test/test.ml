(* open Cfg *)
Printexc.record_backtrace true;;

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

let transltor _ = TrivialCFG.Statement.mkFallThrough
;;

let tA = TrivialBlock.make 1 "A" [] in

let agg = TrivialCFG.aggregate tA (TrivialCFG.BlockSet.singleton tA) false in

print_endline (TrivialCFG.BlockClosure.id agg);

let tB = TrivialBlock.make 1 "B" [] in

TrivialBlock.connect tA (ref tB);

let agg = TrivialCFG.aggregate tA (TrivialCFG.BlockSet.singleton tB) false in

print_endline (TrivialCFG.BlockClosure.id agg)

