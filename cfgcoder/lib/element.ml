module Id = String
module Name = String
module TypeMap = Map.Make(Id)

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

module MakeExp : Exp = struct
  type vname = Name.t
  type fname = Name.t
  type 'a t =
    | UNIT
    | VAR of vname
    | CONSTANT of 'a
    | REF of 'a t
    | DEREF of 'a t
    | OFFSET of (fname * 'a t)
    | APP of 'a t * ('a t array)
 
  (* Helper functions *)
  let mkVar t = VAR t
  let mkConstant c = CONSTANT c
  let mkRef t = REF t
  let mkDeref t = DEREF t
  let mkOffset f t = OFFSET (f, t)
  let mkApp t ta = APP (t, ta)
  let mkUnit () = UNIT
  let to_string _ = "Unimplemented"

end

module type Statement = sig
  module Exp: Exp
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

module MakeStatement (E:Exp) :Statement = struct
  module Exp = E
  type 'a t =
    | Assign of ('a * 'a)
    | Load of ('a * 'a)
    | MutInd of ('a Exp.t * 'a t) list
    | Loop of (('a Exp.t) list * 'a t)
    | Bind of (('a Exp.t) option * 'a t * 'a t)
    | FallThrough (* empty statement *)
    | Dangling (* non'a tinate statement *)
    | Raise of int

  let mkAssign x y = Assign (x, y)
  let mkLoad v ptr = Load (v, ptr)
  let mkMutInd cases = MutInd cases
  let mkLoop ctx ts = Loop (ctx, ts)
  let mkFallThrough _ = FallThrough
  let mkDangling _ = Dangling
  let mkRaise i = Raise i
  let bind v arg app = Bind (v, arg, app)
  let emit _ _ = ()
end
