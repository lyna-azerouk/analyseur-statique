
open Abstract_syntax_tree

module Disjonction = (struct
  (* types *)
  type bound =
  | Int of Z.t
  | INF
  | MINF

  type t = 
  | Iv of bound * bound
  | BOT
  | Union of t * t

(*Utilities *)
let gt_value x y = match x,y with
| MINF, _ | _, INF -> false
| INF, _ | _, MINF -> true 
| Int v1, Int v2 -> Z.gt v1 v2

let neg_bound= function
  | INF -> MINF
  | MINF -> INF
  | Int x -> Int (Z.neg x)

let bound_cmp (a:bound) (b:bound) : int = match a,b with
  | MINF, MINF | INF,INF -> 0
  | MINF,_ | _,INF -> -1
  | INF,_ | _,MINF -> 1
  | Int i, Int j -> Z.compare i j


let bound_min (a:bound) (b:bound) : bound = 
  let x = bound_cmp a b in
    if x=(-1) || x=0 then a
    else b

let bound_max (a:bound) (b:bound) : bound =
  let x = bound_cmp a b in
    if(x=(-1) || x=0) then b
    else a

let minus_one x =
  match x with
  |INF -> INF
  |MINF -> MINF
  |Int a -> Int (Z.sub a Z.one)

let plus_one x =
  match x with
  |INF -> INF
  |MINF -> MINF
  |Int a -> Int (Z.add a Z.one)


let lift2  f x y =match x,y with
  | BOT,_ | _,BOT -> BOT
  | Iv(a,b), Iv (c,d) ->(match a,b,c,d with
                        |Int a,INF,Int c,_| Int a,_,Int c,INF -> Iv (Int (f a c),INF)
                        |Int a,Int b,Int c,Int d -> Iv (Int (f a c),Int (f b d))
                        |MINF,_,_,INF| _,INF,MINF,_| MINF,INF,_,_ | _,_,MINF,INF -> Iv(MINF,INF)
                        |MINF,Int b,_,Int d| _,Int b,MINF,Int d -> Iv (MINF, Int (f b d))
                        |_ -> BOT)
  |_ ->BOT (* Disjonction*)


let top = Iv(MINF, INF)

let bottom = BOT

let is_bottom a =  a=BOT

let const c = Iv(Int c, Int c)

let rand a b = match a, b with
  | _, _ when b < a -> BOT 
  | _, _ -> Iv (Int a, Int b)

let rec subset a b = match a,b with 
    | BOT,_ -> true
    | _, BOT -> false
    | Iv (a,b), Iv(c,d) -> bound_cmp a c >= 0 && bound_cmp b d <= 0
    | Union(i,j), z -> (subset i z) || (subset j z) (* Disjonction*)
    | z, Union(i,j) -> (subset z i) || (subset z j) (* Disjonction*)

let neg = function 
| BOT -> BOT 
| Iv(a, b) -> Iv(neg_bound b, neg_bound a)
|_ -> BOT (* Disjonction*)

let add (x:t) (y:t) : t = lift2 Z.add x y

let sub x y= lift2 Z.add x (neg y)

let rec join a b = match a,b with
| BOT, x | x, BOT -> x
| Iv(INF, MINF), x | x, Iv(INF, MINF) -> x
| Iv(i, j), Iv(k, l) ->  Iv(bound_min i k, bound_max j l)
| Union(i,j), z | z, Union(i,j) -> Union(i, (join j z)) (* Disjonction*)


let rec meet x y =  match x,y with 
  | BOT, _ | _, BOT -> BOT 
  | Iv (a1, b1), Iv (a2, b2) ->
      if gt_value a2 b1 || gt_value a1 b2 then BOT 
      else Iv (bound_max a1 a2, bound_min b1 b2)
  | Union(a,b), z -> join (meet a z) (meet b z) (* Disjonction*)
  | z, Union(a,b) -> join (meet z a) (meet z b) (* Disjonction*)

let  widen x y = match x,y with
  |  BOT,_ | _,BOT -> BOT
  |_  -> if subset x y then y else Union(x, y)

let eq a b = let m = meet a b in m, m

let neq a b = match a, b with 
  |Iv(x, y),Iv( v, w) when (bound_cmp x v) = 0 && (bound_cmp y w) = 0 -> a,b
  |Iv(x, y),Iv( v, _) when (bound_cmp x v) =0 -> Iv(plus_one x, y), b
  | Union(_,_), _ -> a,b (* Disjonction*)
  | _, Union(_,_) -> a,b (* Disjonction*)
  | _, _ ->  let m = meet a b in m, m

let rec print fmt x = match x with
  |Iv(Int a, Int b) -> Format.fprintf fmt "[%a;%a]" Z.pp_print a Z.pp_print b
  |Iv(MINF, Int b) -> Format.fprintf fmt "[-∞;%a]" Z.pp_print b
  |Iv(Int a, INF) -> Format.fprintf fmt "[%a;+∞]" Z.pp_print a
  |Iv(MINF, INF) -> Format.fprintf fmt "[-∞;+∞]"
  | Union((a:t), (b:t)) -> print fmt a; Format.fprintf fmt " U "; print fmt b (* Disjonction*)
  | _ -> Format.fprintf fmt "Disjunctions.print"

      
let unary x op = match op with
| AST_UNARY_PLUS  -> x
| AST_UNARY_MINUS -> neg x

let binary x y op = match op with
| AST_PLUS     -> add x y
| AST_MINUS    -> sub x y
| _ -> BOT

let compare x y op = match op with
| AST_EQUAL         -> eq x y
| AST_NOT_EQUAL     -> neq x y
|_ -> BOT,BOT


let bwd_unary x op r = match op with
| AST_UNARY_PLUS  -> meet x r
| AST_UNARY_MINUS -> meet x (neg r)

      
let bwd_binary x y op r = match op with
| AST_PLUS ->
    meet x (sub r y), meet y (sub r x)
| AST_MINUS ->
    meet x (add y r), meet y (sub y r)
|_ -> BOT,BOT

end)