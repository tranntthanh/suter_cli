module type Exp = sig
  type vname
  type fname
  type 'a t
  
  (* Helper functions *)
  val mkVar : vname -> 'a t
  val mkConstant : 'a -> 'a t
  val mkRef : 'a t -> 'a t 
  val mkDeref : 'a t -> 'a t
  val mkApp : 'a t -> 'a t array -> 'a t
  val mkOffset : fname -> 'a t -> 'a t
  val mkUnit : unit -> 'a t
  val to_string : 'a t -> string 
end

module type Statement = sig
  module Exp : Exp
  type 'a t
  val mkAssign: 'a -> 'a -> 'a t 
  val mkLoad: 'a -> 'a -> 'a t 
  val mkMutInd: ('a Exp.t * 'a t) list -> 'a t 
  val mkLoop: ('a Exp.t) list -> 'a t -> 'a t
  val mkFallThrough: unit -> 'a t
  val mkDangling: unit -> 'a t
  val mkRaise: int -> 'a t
  val bind: ('a Exp.t) option -> 'a t -> 'a t -> 'a t
  val emit: char Stream.t -> 'a t -> unit
end

module MakeExp : Exp

module MakeStatement:
  functor (E:Exp) -> Statement
