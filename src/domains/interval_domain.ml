(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
  Antoine Miné 2015
*)

(* 
   The Iv domain
 *)


 open Abstract_syntax_tree
  open Value_domain

 type bound =
 | PINF
 | MINF
 | Int of Z.t

type intervalTyp = 
 | Iv of bound*bound
 | BOT 


module type IntervalSig = sig
  type t = intervalTyp

  include VALUE_DOMAIN with type t := t
  val is_pair : bound -> bool
  val fst : t -> bound
  val snd : t -> bound
  val plus_one : bound -> bound
  val minus_one : bound -> bound
  val interval : bound -> bound -> t
end



 module Intervals : IntervalSig = (struct
  type t = intervalTyp
 (* types *)
   (* ***** *)

(* Fonction utilitaire: ***********************************************)
  let bound_cmp (a:bound) (b:bound) : int = match a,b with
  | MINF,MINF | PINF,PINF -> 0
  | MINF,_ | _,PINF -> -1
  | PINF,_ | _,MINF -> 1
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
    |PINF -> PINF
    |MINF -> MINF
    |Int a -> Int (Z.sub a Z.one)

  let plus_one x =
    match x with
    |PINF -> PINF
    |MINF -> MINF
    |Int a -> Int (Z.add a Z.one)

  let neg_bound= function
    | PINF -> MINF
    | MINF -> PINF
    | Int x -> Int (Z.neg x)

  (* lift binary arithmetic operations *)
  let lift2  f x y =match x,y with
  | BOT,_ | _,BOT -> BOT
  | Iv(a,b), Iv (c,d) ->(match a,b,c,d with
                        |Int a,PINF,Int c,_| Int a,_,Int c,PINF -> Iv (Int (f a c),PINF)
                        |Int a,Int b,Int c,Int d -> Iv (Int (f a c),Int (f b d))
                        |MINF,_,_,PINF| _,PINF,MINF,_| MINF,PINF,_,_ | _,_,MINF,PINF -> Iv(MINF,PINF)
                        |MINF,Int b,_,Int d| _,Int b,MINF,Int d -> Iv (MINF, Int (f b d))
                        |_ -> BOT)


  let neg = function 
    | BOT -> BOT 
    | Iv(a, b) -> Iv(neg_bound b, neg_bound a)
            

  let add (x:t) (y:t) : t = lift2 Z.add x y
  let sub x y= lift2 Z.add x (neg y)

  let mul (x: t) (y: t) : t =
    match x, y with
    | Iv(Int a, Int b), Iv(Int c, Int d) ->
      let min_bound = Z.min (Z.mul a c) (Z.mul a d) in
      let max_bound = Z.max (Z.mul b c) (Z.mul b d) in
      Iv(Int min_bound, Int max_bound)
    | _, _ -> Iv(Int Z.zero, Int Z.zero)
  
  let div (x: t) (y: t) : t =
    match x, y with
    | _, Iv(_, Int y1) when Z.equal y1 Z.zero -> BOT
    | Iv(Int a, Int b), Iv(Int c, Int d) when Z.compare c Z.one >= 0 ->
      let min_bound = Z.min (Z.div a c) (Z.div a d) in
      let max_bound = Z.max (Z.div b c) (Z.div b d) in
      Iv(Int min_bound, Int max_bound)
    | Iv(Int a, Int b), Iv(Int c, Int d) ->
      let min_bound = Z.min (Z.div b c) (Z.div b d) in
      let max_bound = Z.max (Z.div a c) (Z.div a d) in
      Iv(Int min_bound, Int max_bound)
    | _, _ -> BOT

      (* comparison for values *)
  let gt_value x y = match x,y with
      | MINF, _ | _, PINF -> false
      | PINF, _ | _, MINF -> true 
      | Int v1, Int v2 -> Z.gt v1 v2

  (* intersection (filters) *)  
  let meet x y =  match x,y with 
    | BOT, _ | _, BOT -> BOT 
    | Iv (a1, b1), Iv (a2, b2) ->
        if gt_value a2 b1 || gt_value a1 b2 then BOT 
        else Iv (bound_max a1 a2, bound_min b1 b2)
  
  let eq a b =
    let m = meet a b in
    match m with
    | BOT -> BOT, BOT
    | Iv (start, end_) -> Iv (start, end_), Iv (start, end_)

  let neq a b = match a, b with (* explained by zacky*)
    |Iv(x, y),Iv( v, w) when (bound_cmp x v) = 0 && (bound_cmp y w) = 0 -> a,b
    |Iv(x, y),Iv( v, _) when (bound_cmp x v) =0 -> Iv(plus_one x, y), b
    | _, _ ->  let m = meet a b in m, m

  let geq a b = match a, b with  (* explained by zacky*)
    |Iv(x, y),Iv( v, w) when (bound_cmp y v) >= 0-> Iv(bound_max x v, y), Iv(v, bound_min y w)
    |_ ,_  -> BOT, BOT

  let gt a b = match a, b with  (* explained by zacky*)
    |Iv(x, y), Iv(v, w) when (bound_cmp y v) >= 1 -> Iv(bound_max x (plus_one v), y), Iv(v, bound_min (minus_one y) w)
    |_ ,_  ->  BOT, BOT

  
  (* let lett a b = match a, b with  (* explained by zacky*)
    |Iv(x, y), Iv(v, w) when (bound_cmp y v) = 1 || (bound_cmp y v) = 0-> Iv(x, bound_min y w), Iv(bound_max x v, w)
    |_ ,_  ->  BOT, BOT

  let lt a b = match a, b with  (* explained by zacky*)
    |Iv(x, y), Iv(v, w) when (bound_cmp y v) = 1 -> Iv(x, bound_min y (minus_one w)), Iv(bound_max x (plus_one v), w)
    |_ ,_  ->  BOT, BOT *)

(* Fonction utilitaire: ***********************************************)
  let bound_to_string (x:bound) = match x with 
    |Int x ->  Z.to_string x
    |PINF -> "+∞"
    |MINF -> "-∞"

  let print fmt (x:t)=  match x with 
  | Iv(x, y) -> Format.fprintf fmt "[%s;%s]" (bound_to_string x) (bound_to_string y)
  | BOT  -> Format.fprintf fmt "botttom"

  let top  = Iv( MINF, PINF)

  let bottom = BOT

  let const c = Iv (Int c, Int c)

  let is_bottom a =  a=BOT

  let rand a b =
    match a, b with
    | _, _ when b < a -> BOT (* Handle case where b < a *)
    | _, _ -> Iv (Int a, Int b) (* Handle other cases *)
    
  
  let unary x op = match  op with 
    | AST_UNARY_MINUS -> neg x
    | AST_UNARY_PLUS  ->  x

  let binary x y op = match op with 
    | AST_PLUS -> add x y
    | AST_MINUS -> sub x y 
    | AST_MULTIPLY -> mul x y
    | AST_DIVIDE   -> div x y

(*TO DO: correct this function ********************)
  let compare x y op  =  match op with 
    | AST_NOT_EQUAL -> neq x y
    | AST_EQUAL -> eq x y
    | AST_GREATER_EQUAL -> geq x y
    | AST_GREATER ->gt x y
    | AST_LESS_EQUAL    -> let y',x' = geq y x in x',y'
    | AST_LESS          -> let y',x' = gt y x in x',y'


  let subset a b = match a,b with 
    | BOT,_ -> true
    | _, BOT -> false
    | Iv (a,b), Iv(c,d) -> bound_cmp a c >= 0 && bound_cmp b d <= 0

  let join a b = match a, b with 
    | BOT, x | x, BOT -> x
    | Iv(PINF, MINF), x | x, Iv(PINF, MINF) -> x
    | Iv(i, j), Iv(k, l) ->  Iv(bound_min i k, bound_max j l)

  let bwd_binary x y op r = match op with 
    | AST_PLUS ->
      meet x (sub r y), meet y (sub r x)
    | AST_MINUS ->
      meet x (add y r), meet y (sub x r)
    | AST_MULTIPLY ->
      let contains_zero o = subset (const Z.zero) o in
      (if contains_zero y && contains_zero r then x else meet x (div r y)),
      (if contains_zero x && contains_zero r then y else meet y (div r x))
    |_  -> x,y 

  let bwd_unary x op r = match op with
    | AST_UNARY_PLUS  -> meet x r
    | AST_UNARY_MINUS -> meet x (neg r)

  let max_value x y = not (gt_value y x)

  let widen x y = match x,y with
    | BOT, _ -> y 
    | _, BOT -> x
    | Iv (a, b), Iv (c, d) ->
      let a' = if max_value c a then a else MINF in
      let b' = if max_value b d then b else PINF in Iv(a',b')
  

  let is_pair x = match x with
    | (Int a) -> (Z.erem a (Z.of_int 2))=Z.zero
    |PINF -> true
    |MINF -> true

  let fst x = match x with
    | Iv(a, _) -> a
    | _ -> failwith "Not a pair"

  let snd x = match x with
    | Iv(_, b) -> b
    | _ -> failwith "Not a pair"

  let interval (a: bound) (b: bound) : t = 
      match a, b with
      | Int a, Int b  when a > b  ->  BOT
      | Int a, Int b  -> Iv (Int a, Int b)
      |MINF , Int b ->  Iv (MINF, Int b)
      |Int a, PINF ->  Iv (Int a, PINF)
      |MINF, PINF ->  Iv (MINF, PINF)
      | _ -> BOT


 end )