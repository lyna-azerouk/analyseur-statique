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
    | Int of Z.t
    | PINF
    | MINF

  type t = 
    | Iv of bound*bound
    | BOT 


  let bound_to_string (x:bound) = match x with 
    |Int a  -> Z.to_string a 
    |PINF -> "INF"
    |MINF -> "MINF"

  let print fmt (x:t)=  match x with 
  | BOT  -> Format.fprintf fmt "botttom"
  | Iv(x, y) -> Format.fprintf fmt "[%s;%s]" (bound_to_string x) (bound_to_string y)




  let top  = Iv( MINF, PINF)

  let bottom =BOT

  let const c = Iv (Int c, Int c)

  let rand a v =  match a, v with 
    |_ , _-> BOT


  let meet a b :t =   match a,b with 
    _ -> a

  let eq a b = let m = meet a b in m, m

  let  subset a b = match a,b with 
    |_ ->true

  let is_bottom a =  match a with 
    |_ -> true

  let unary x op = match x, op with 
    |_, _ -> x

  let binary x y op = match x,y,op with 
    |_, _, _ ->x

  let widen x y = match x, y with 
    |_ -> BOT

  let compare x y op  =  match op with 
    |AST_EQUAL -> eq x y
    |_ -> eq x y
  
  let bwd_unary x op r =  match x,op,r with 
    | _,_,_ -> x

  let bwd_binary x y op r  =  match x,y,op,r with 
    |_,_,_,_ -> x, y

  let join x y = match x, y with 
  |_ -> x

 end : VALUE_DOMAIN)