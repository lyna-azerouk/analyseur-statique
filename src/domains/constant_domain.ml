(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)

(*
   The constant domain.
 *)

open Abstract_syntax_tree
open Value_domain


module Constants = (struct


  (* types *)
  (* ***** *)


  (* type of abstract values *)
  type t =
    | Cst of Z.t  (* the set is a single value (constant) *)
    | BOT         (* the set is empty (not reachable) *)
    | TOP         (* the set of all integers (not constant)  *)


  (* utilities *)
  (* ********* *)


  (* lift unary arithmetic operations, from integers to t *)
  let lift1 f x =
    match x with
    | BOT -> BOT
    | TOP -> TOP
    | Cst a -> Cst (f a)

  (* lift binary arithmetic operations *)
  let lift2 f x y =
    match x,y with
    | BOT,_ | _,BOT -> BOT
    | TOP,_ | _,TOP -> TOP
    | Cst a, Cst b -> Cst (f a b)


  (* interface implementation *)
  (* ************************ *)


  (* unrestricted value *)
  let top = TOP

  (* bottom value *)
  let bottom = BOT

  (* constant *)
  let const c = Cst c

  (* interval *)
  let rand x y =
    if x=y then Cst x
    else if x<y then TOP
    else BOT


  (* arithmetic operations *)

  let neg = lift1 Z.neg

  let add = lift2 Z.add

  let sub = lift2 Z.sub

  let mul x y= match x,y with
	| TOP, Cst a |Cst a, TOP when  a= Z.zero -> Cst Z.zero
	| _ -> lift2 Z.mul x y

  let div a b =
    match a,b with
    |_,Cst y when (y==Z.zero) -> BOT
    |Cst y,TOP when (y==Z.zero)->Cst Z.zero
    |_,_ -> lift2 Z.div a b


  (* set-theoretic operations *)

  let join a b = match a,b with
  | BOT,x | x,BOT -> x
  | Cst x, Cst y when x=y -> a
  | _ -> TOP

  let meet a b = match a,b with
  | TOP,x | x,TOP -> x
  | Cst x, Cst y when x=y -> a
  | _ -> BOT


  (* no need for a widening as the domain has finite height; we use the join *)
  let widen = join


  (* comparison operations (filters) *)

  let eq a b = match a,b with
  | Cst x, Cst y -> if Z.equal x y then a, b else BOT, BOT
  | TOP, Cst x -> Cst x , Cst x
  | Cst x , TOP -> Cst x , Cst x
  | BOT,x | x,BOT -> x, BOT
  | _ -> TOP, TOP

  let neq a b = match a,b with
  | BOT,x | x,BOT -> x, BOT
  | Cst x, Cst y -> if not (Z.equal x y) then a, b else BOT, BOT
  | _ -> TOP, TOP

  let geq a b = match a,b with
    | Cst x, Cst y -> if Z.geq x y then a, b else BOT, TOP 
    | _ -> a, b
        
  let gt a b = match a,b with
	  | Cst x, Cst y when Z.compare x y > 0 -> a, b
    | x, TOP ->  x, TOP
    | TOP, x -> TOP, x
    | BOT, x -> BOT, x
    | _ -> BOT, BOT


  (* subset inclusion of concretizations *)
  let subset a b = match a,b with
  | BOT,_ | _,TOP -> true
  | Cst x, Cst y -> x=y
  | _ -> false

  (* check the emptiness of the concretization *)
  let is_bottom a =
    a=BOT

  (* print abstract element *)
  let print fmt x = match x with
  | BOT -> Format.fprintf fmt "⊥"
  | TOP -> Format.fprintf fmt "⊤"
  | Cst x -> Format.fprintf fmt "{%s}" (Z.to_string x)


  (* operator dispatch *)

  let unary x op = match op with
  | AST_UNARY_PLUS  -> x
  | AST_UNARY_MINUS -> neg x

  let binary x y op = match op with
  | AST_PLUS     -> add x y
  | AST_MINUS    -> sub x y
  | AST_MULTIPLY -> mul x y
  | AST_DIVIDE   -> div x y

  let compare x y op = match op with
  | AST_EQUAL         -> eq x y
  | AST_NOT_EQUAL     -> neq x y
  | AST_GREATER_EQUAL -> geq x y
  | AST_GREATER       -> gt x y
  | AST_LESS_EQUAL    -> let y',x' = geq y x in x',y'
  | AST_LESS          -> let y',x' = gt y x in x',y'


  let bwd_unary x op r = match op with
  | AST_UNARY_PLUS  -> meet x r
  | AST_UNARY_MINUS -> meet x (neg r)


  let bwd_binary x y op r = match op with

  | AST_PLUS ->
      (* r=x+y => x=r-y and y=r-x *)
      meet x (sub r y), meet y (sub r x)

  | AST_MINUS ->
      (* r=x-y => x=y+r and y=x-r *)
      meet x (add y r), meet y (sub x r)

  | AST_MULTIPLY ->
      (* r=x*y => (x=r/y or y=r=0) and (y=r/x or x=r=0)  *)
      let contains_zero o = subset (const Z.zero) o in
      (if contains_zero y && contains_zero r then x else meet x (div r y)),
      (if contains_zero x && contains_zero r then y else meet y (div r x))

  | AST_DIVIDE ->
      (* this is sound, but not precise *)
      x, y

end : VALUE_DOMAIN)
