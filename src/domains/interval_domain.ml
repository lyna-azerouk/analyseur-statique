(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
  Antoine Miné 2015
*)

(* 
   The interval domain
 *)


 open Abstract_syntax_tree
 open Value_domain


 module Intervals = (struct

 (* types *)
   (* ***** *)
   type bound =
   | PINF
   | MINF
   | Cst of Z.t

 type t = 
   | Iv of bound*bound
   | BOT 

(* Fonction utilitaire: ***********************************************)
  (* lift binary arithmetic operations *)
  let lift1 f x  =match x with
  | BOT-> BOT
  | Iv(a,b) -> (match a, b with
              |Cst x, Cst y when (Z.geq x Z.zero && Z.geq y Z.zero )-> if Z.gt (f x) (f y) then Iv(Cst (f y),  Cst(f x)) else  Iv(Cst (f x),  Cst(f y)) 
              | _, _  -> Iv(a, b))

  (* lift binary arithmetic operations *)
  let lift2  f x y =match x,y with
  | BOT,_ | _,BOT -> BOT
  | Iv(a,b), Iv (c,d) ->(match a,b,c,d with
                        |Cst a,PINF,Cst c,_| Cst a,_,Cst c,PINF -> Iv (Cst (f a c),PINF)
                        |Cst a,Cst b,Cst c,Cst d -> Iv (Cst (f a c),Cst (f b d))
                        |MINF,_,_,PINF| _,PINF,MINF,_| MINF,PINF,_,_ | _,_,MINF,PINF -> Iv(MINF,PINF)
                        |MINF,Cst b,_,Cst d| _,Cst b,MINF,Cst d -> Iv (MINF, Cst (f b d))
                        |_ -> BOT)

  (* arithmetic operations *)
  let add (x:t) (y:t) : t = lift2 Z.add x y
  let sub x y= lift2 Z.sub x y
  let mul (x: t) (y: t) : t =
    match x, y with
    | Iv(Cst a, Cst b), Iv(Cst c, Cst d) ->
      let min_bound = Z.min (Z.mul a c) (Z.mul a d) in
      let max_bound = Z.max (Z.mul b c) (Z.mul b d) in
      Iv(Cst min_bound, Cst max_bound)
    | _, _ -> Iv(Cst Z.zero, Cst Z.zero)
  
  let div (x: t) (y: t) : t =
    match x, y with
    | _, Iv(_, Cst y1) when Z.equal y1 Z.zero -> BOT
    | Iv(Cst a, Cst b), Iv(Cst c, Cst d) when Z.compare c Z.one >= 0 ->
      let min_bound = Z.min (Z.div a c) (Z.div a d) in
      let max_bound = Z.max (Z.div b c) (Z.div b d) in
      Iv(Cst min_bound, Cst max_bound)
    | Iv(Cst a, Cst b), Iv(Cst c, Cst d) ->
      let min_bound = Z.min (Z.div b c) (Z.div b d) in
      let max_bound = Z.max (Z.div a c) (Z.div a d) in
      Iv(Cst min_bound, Cst max_bound)
    | _, _ -> BOT

  let neg  x = lift1 Z.neg x
  (* comparison operations (filters) *)
  let meet x y :t = match x,y with  (*Dans ce cs meet prend en paramettre deux intervals*)
  |Iv(Cst a, Cst b), Iv(Cst c, Cst d) -> Iv(Cst (Z.max a c),  Cst (Z.max b d))
  |_ , _ -> x
  

  let eq a b = let m = meet a b in m, m
  let neq a b = let m = meet a b in m, m
  let geq a b = let m = meet a b in m, m
  let gt a b = let m = meet a b in m, m


(* Fonction utilitaire: ***********************************************)

  let bound_to_string (x:bound) = match x with 
    |Cst x ->  Z.to_string x
    |PINF -> "INF"
    |MINF -> "MINF"

  let print fmt (x:t)=  match x with 
  | Iv(x, y) -> Format.fprintf fmt "[%s;%s]" (bound_to_string x) (bound_to_string y)
  | BOT  -> Format.fprintf fmt "botttom"

  let top  = Iv( MINF, PINF)

  let bottom =BOT

  let const c = Iv (Cst c, Cst c)

  let is_bottom a =  a=BOT

  let rand a b =
    match a, b with
    | _, _ when b < a -> BOT (* Handle case where b < a *)
    | _, _ -> Iv (Cst a, Cst b) (* Handle other cases *)

  let unary x op = match  op with 
    | AST_UNARY_MINUS -> neg x
    | AST_UNARY_PLUS  ->  x

  let binary x y op = match op with 
    | AST_PLUS -> add x y
    | AST_MINUS -> sub x y 
    | AST_MULTIPLY -> mul x y
    | AST_DIVIDE   -> div x y

  let compare x y op  =  match op with 
    | AST_EQUAL -> eq x y
    | AST_NOT_EQUAL     -> neq x y
    | AST_GREATER_EQUAL -> geq x y
    | AST_GREATER -> gt x y
    | AST_LESS_EQUAL    -> let y',x' = geq y x in x',y'
    | AST_LESS  -> let y',x' = gt y x in x',y'


  let bwd_binary x y op r = match op, r with 
  |_ ,_  -> x,y 

  let subset a b = match a,b with 
    |_ ->true



  let widen x y = match x, y with 
    |_ -> BOT

  
  let bwd_unary x op r =  match x,op,r with 
    | _,_,_ -> x


  let join x y = match x, y with 
  |_ , _-> x

 end : VALUE_DOMAIN)