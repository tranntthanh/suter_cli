open Element

module type Block = sig
  type elt
  type t
  val index: t -> int
  val id: t-> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val next: t -> t list
  val elements: t -> elt list
end

module type Translator = sig
  val translate: 'a -> 'b
end

module Graph (B:Block) = struct
  let ready_str ready_track =
      "ready |" ^ List.fold_left (fun acc c-> acc ^ " -> " ^
            match c with | Some c -> B.id c | None -> "<>")
        "" ready_track

  let dfs entries extend_callback contract_callback stop_callback log_callback =
    let ready_track = entries in
    let rec step_once ready_stack = begin
      (* Invariance: if x∈ready_stack then x∈closure *)
      match ready_stack with
      | [] ->
        log_callback "stop" @@ ready_str ready_track;
        stop_callback ()
      | Some b :: tl -> (* Remove it from the scan list *)
        log_callback "extend" @@ ready_str ready_track;
        let ready_track = if extend_callback b then
          (* Normal case, a new node needs to be tracked *)
          List.fold_left (fun acc x ->
            Some x :: (None :: acc)
          ) tl (B.next b)
        else
          tl
        in step_once ready_track
      | None :: tl ->
        log_callback "contract" @@ ready_str ready_track;
        contract_callback ();
        step_once tl
    end in
    step_once ready_track
end

module AggregateSet (B: Block) = struct

  type elt = B.t

  module BlockMap = Map.Make(B)
  module BlockSet = Set.Make(B)
  (*
   * Aggregate is a set of blocks that all blocks in int are 
   * connected with each other
   *
   *  |<----------------- top aggregate scope ------------->| 
   *
   *  +------ aggregate set -------+
   *  |                            |
   *  |  loop_block -> loop_block -+--> merge_block --------+
   *  |    ↑            ↓          |                        |
   *  |  loop_block <- loop_block -+--> merge_block --------+
   *  |                            |
   *  +----------------------------+
   *)
  type t = {
    blocks: BlockSet.t;
    aggregate_map: (BlockSet.t ref) BlockMap.t;
  }

  let index b =
    BlockSet.fold (fun b acc -> acc * (B.index b)) b.blocks 1

  let id b =
    let r = BlockSet.fold (fun b acc -> B.id b ^ " " ^ acc) b.blocks "" in
    "[ bset: " ^ r ^ " ]"

  let make bset map = { blocks=bset; aggregate_map=map; }
  let with_in_closure t b = BlockMap.mem b t.aggregate_map

  let find_aggro b t =
    let bset = BlockMap.find b t.aggregate_map in
    make !bset t.aggregate_map


  let compare x y = (index x) - (index y)
  let equal x y = ((index x) = (index y))
  let next bset =
    snd @@ BlockSet.fold (fun b acc ->
      let nexts = B.next b in
      List.fold_left (fun (acc,s) b ->
        if BlockMap.mem b bset.aggregate_map then
          let r = BlockMap.find b bset.aggregate_map in
          if List.mem r acc then (acc, s)
          else (r :: acc, (make !r bset.aggregate_map) :: s)
        else (acc,s)
      ) acc nexts
    ) bset.blocks ([],[])

  let elements bset = BlockSet.elements bset.blocks

  (*
   * Based on an underlying block cfg structure,
   * We would like to contract all the loop into one complex node.
   *)
  let iter f a = 
    BlockMap.iter (fun _ v ->
      let aggo = make !v a.aggregate_map in
      f aggo 
    ) a.aggregate_map

end

(* The Basic Blocks we would like to translate *)
module Make (BasicBlock: Block) = struct
  module Exp = MakeExp
  module Statement = MakeStatement(Exp)
  module BlockSet = Set.Make(BasicBlock) 
  module BlockMap = Map.Make(BasicBlock)
  module BGraph = Graph(BasicBlock)
  module BlockClosure = AggregateSet(BasicBlock)
  module BlockClosureGraph = Graph(BlockClosure)
  module AggroSet = Set.Make(BlockClosure)

  type error =
  | MultiEntry of BlockSet.t

  exception CFGError of (error * BlockClosure.t)

  (*
   * Given an entry of type B.t compute entry' of type t.
   * So that entry' is a aggregate of entry and
   * there is no loop starting from entry' in Graph(t)
   *)
  let aggregate entry blockset entry_as_exit =
    let aggregate_map = ref BlockMap.empty in
    let path = ref [] in

    (* get the tail list which starts with x = c *)
    let rec get_tl_for c ls = match ls with
      | [] -> []
      | h::tl when BasicBlock.equal c h -> tl
      | _::tl -> get_tl_for c tl
    in

    (*
     * Suppose that path = x_1, x_2, ... x_k,
     * then, we check whether there is alreay c inside the path.
     * If this is the case, then we mark
     *   path = x_1, x_2,  ... c
     * to be the tl_for path and aggregate them together
     * Otherwise, we add c at the begin of the path
     *   path = c, x_1, x_2, ..., x_k
     *)
    let extend_callback c =
      if entry_as_exit || (not (BlockSet.mem c blockset)) then
        false
      else begin (* A valid extention point *)
        let tl_for_c = get_tl_for c (List.rev !path) in
        match tl_for_c with
        | [] -> (* Nothing to aggregate *) 
          if not (BlockMap.mem c !aggregate_map) then
            aggregate_map := BlockMap.add c
              (ref (BlockSet.singleton c)) !aggregate_map;
          path := c :: !path;
          true
        | ls -> (* Detect a loop here *)
          let bset = List.fold_left (fun acc c ->
            BlockSet.union acc (!(BlockMap.find c !aggregate_map))
          ) !(BlockMap.find c !aggregate_map) ls in
          BlockSet.iter (fun c ->
            aggregate_map := BlockMap.add c (ref bset) !aggregate_map
          ) bset;
          false
      end
    in

    let stop_callback () = () in
    let contract_callback () = path := List.tl !path in
    let log_callback state hint =
      Format.printf "%s\n" hint;
      Format.printf "%s |%s\n" state (List.fold_left (
        fun acc c-> acc ^ " -> " ^ BasicBlock.id c
      ) "" !path)
    in

    
    BGraph.dfs [Some entry; None] extend_callback
        contract_callback stop_callback log_callback;

    (* now the aggregate map is constructed and we can return the first entry *)
    BlockClosure.make !(BlockMap.find entry !aggregate_map) !aggregate_map

  type 'a merge_point =
    | Merge of 'a
    | Diverge of 'a list

  (* get exits of an entry block aggro set *) 
  let get_merge_point entry_aggro =

    let path: BlockClosure.t list ref = ref [] in
    let mps: BlockClosure.t list ref = ref [] in
    let natual_exits = ref AggroSet.empty in
    (*
     * Since this is called after aggregrate
     * there should be no loop in the graph.
     *
     * Suppose that path = x_1, x_2, ... x_k,
     * then we add c at the begin of the path
     * new patch = c, x_1, x_2, ..., x_k
     *)
    let extend_callback c = path := c :: !path; true in
    let stop_callback () = () in
    let contract_callback () =
    (* Update the merge point candidates *)
      let mps' = match !mps with
      | [] -> List.rev !path
      | ts ->
        List.fold_left (fun acc (aggro:BlockClosure.t) ->
          if List.mem aggro ts then
            (* merge closure from new path *)
            acc @ [aggro]
          else acc
        ) [] (List.rev !path) in
      mps := mps';
      let c = List.hd !path in
      natual_exits := AggroSet.add c !natual_exits
    in

    let log_callback state hint =
      Format.printf "%s\n" hint;
      Format.printf "%s |%s\n" state (List.fold_left (
        fun acc c-> acc ^ " -> " ^ BlockClosure.id c
      ) "" !path)
      in


    BlockClosureGraph.dfs [Some entry_aggro] extend_callback
        contract_callback stop_callback log_callback;

    match !mps with
    | [] -> assert false (* This can not happen since hd = block *)
    | _ :: tl -> match tl with
      | [] -> Diverge (AggroSet.elements !natual_exits)
      | bset :: _ -> Merge bset

  (* -- end of get_merge_point -- *)

  (*
   * Analysis a closure and make into a linear sequence of statements
   *)
  let rec trace closure previous entry_aggro merge_aggro target translator
    : (BlockSet.t * 'b Statement.t) =

    let is_merge_aggro a =
        match merge_aggro with
        | None -> false
        | Some a' -> BlockClosure.equal a' a
    in

    let aggro = BlockClosure.find_aggro target closure in
    let exit_aggros = get_merge_point aggro in
    let exits, (statement:'a Statement.t) =
        trace_within target aggro translator in
    let loop _ _ = false in
    match exit_aggros with
    | Diverge _ (* blocks here are only for debug purpose *) ->
      (*
       * Non of the blocks should stay in the targetrent closure
       * except loop back
       *)
      if loop entry_aggro exits then
        exits, Statement.mkLoop [] (Statement.bind None previous statement)
      else
        exits, Statement.bind None previous statement
    | Merge aggro when is_merge_aggro aggro ->
      (* When we already reach the merge point *)
        exits, Statement.bind None previous statement
    | Merge aggro ->
      begin
        (* We have not reach the merge_point *)
        let exits, branchs = List.fold_left (fun (es, ss) b ->
          let (exits, statement) =
          if (BlockClosure.with_in_closure closure b) then
            trace closure (Statement.mkFallThrough ()) entry_aggro
                (Some aggro) b translator
          else
            (* something not in closure, contradict with
             * merge assumption *)
            assert false
          in
          BlockSet.union es exits, ss @ [(Statement.Exp.mkUnit (), statement)]
        ) (BlockSet.empty, []) (BlockClosure.elements aggro) in
        let catch = Statement.mkMutInd branchs in
        match BlockSet.elements exits with
        | [x] ->
          let (exists, s) = trace closure catch entry_aggro
              merge_aggro x translator in
          exists, Statement.bind None statement s
        | _ -> raise @@ CFGError ((MultiEntry exits), aggro)
      end 
       

  (* Trace the entry block all the way to exit*) 
  and trace_within entry aggro translator =
    assert (BlockSet.mem entry aggro.blocks);
    match BlockSet.elements aggro.blocks with
    | [] -> assert false
    | [hd] -> (* hd must equal to entry *)
      BlockSet.of_list (BasicBlock.next hd), translator hd
    | _ -> begin
        let aggro = aggregate entry aggro.blocks true in
        trace aggro (Statement.mkFallThrough ())
          (BlockClosure.find_aggro entry aggro)
          None entry translator
      end
  (* ---- end of rec trace ---- *)

  let debug aggro =
    BlockClosure.iter (fun aggo ->
      let aggs = BlockClosure.next aggo in
      let nexts = List.fold_left (fun acc a ->
        BlockClosure.id a ^ ";" ^ acc 
      ) "" aggs in
      print_endline @@ (BlockClosure.id aggo) ^ " -> " ^ nexts
    ) aggro

  let emitter _ = ()
end
