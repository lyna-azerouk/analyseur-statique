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
  let lift2  f x y =match x,y with
  | BOT,_ | _,BOT -> BOT
  | Iv (a,b),Iv (c,d) ->(match a,b,c,d with
                        |Cst a,PINF,Cst c,_| Cst a,_,Cst c,PINF -> Iv (Cst (f a c),PINF)
                        |Cst a,Cst b,Cst c,Cst d -> Iv (Cst (f a c),Cst (f b d))
                        |MINF,_,_,PINF| _,PINF,MINF,_| MINF,PINF,_,_ | _,_,MINF,PINF -> Iv(MINF,PINF)
                        |MINF,Cst b,_,Cst d| _,Cst b,MINF,Cst d -> Iv (MINF, Cst (f b d))
                        |_ -> BOT)
          
  (* arithmetic operations *)

  let add (x:t) (y:t) : t = lift2 Z.add x y

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

  let binary x y op = match op with 
    |AST_PLUS -> add x y
    |_  ->x

  let meet a b :t = match a,b with  (*Dans ce cs meet prend en paramettre deux intervals*)
    |_ -> BOT

  let bwd_binary x y op r = match op, r with 
  |_ ,_  -> x,y 

  let eq a b = let m = meet a b in m, m

  let subset a b = match a,b with 
    |_ ->true

  let unary x op = match x, op with 
    |_, _ -> x


  let widen x y = match x, y with 
    |_ -> BOT

  let compare x y op  =  match op with 
    |AST_EQUAL -> eq x y
    |_ -> eq x y
  
  let bwd_unary x op r =  match x,op,r with 
    | _,_,_ -> x


  let join x y = match x, y with 
  |_ , _-> x

 end : VALUE_DOMAIN)