
 open Abstract_syntax_tree
 open Value_domain
 
 
 module Parity = (struct
 
   (* type of abstract values *)
  type t =
    | BOT
    | Even
    | Odd
    | TOP
 
    (* unrestricted value *)
   let top = TOP
 
   (* bottom value *)
   let bottom = BOT
 
   (* constant *)
   let const c = if Z.is_even c then Even else Odd
 
   (* interval *)
   let rand x y =
     if x=y then const x
     else  (if y>x then TOP else BOT)

  let neg  x =  match x with
    | Even -> Even
    | Odd -> Odd
    | BOT -> BOT
    | TOP -> TOP
 
  let add x y = match x,y with 
    | Even, Even->  Even
    | Odd, Odd ->  Even
    | Even, Odd ->  Odd
    | Odd, Even ->  Odd
    | _,_ -> TOP
 
  let sub x y= add x (neg y)
 
  let mul x y  = match x,y with
    |_, Even-> Even
    | _, Odd | Odd, _ -> Odd
    | Even, _ -> Even
    |BOT, _ |_,BOT-> BOT
    |_  ->TOP

  let div a b = match a,b with
    | BOT,_ | _,BOT -> BOT
    | Even, Odd -> Even
    | Odd, Odd -> Odd
    | _ -> TOP

  let join a b = match a,b with
    | BOT,x | x,BOT -> x
    | TOP,_ | _,TOP -> TOP
    | Even, Even -> Even
    | Odd, Odd -> Odd
    | _ -> TOP
 
  let meet a b = match a,b with
  | TOP, x | x, TOP -> x
  | Even, Even -> Even
  | Odd, Odd -> Odd 
  | _ -> BOT  

 
  let widen = join

  let eq x y = match x,y with
    | BOT, _ | _, BOT -> BOT, BOT
    | TOP, a | a , TOP -> a, a
    | Even, Odd | Odd, Even -> BOT, BOT
    | _ -> x,y

  let neq x y = match x,y with
    | _ -> x,y
   
  let gt x y = match x,y with 
    | BOT, _ -> BOT, BOT
    | _ -> x,y
 
  let geq x y = match x,y with 
    | _ -> x,y
 
  let subset x y = match x,y with
  | BOT,_ | _,TOP -> true
  | Even, Even | Odd, Odd -> true 
  | _ -> false 

  let is_bottom a =  a = BOT
 
   (* print abstract element *)
  let print fmt x = match x with
   | BOT -> Format.fprintf fmt "⊥"
   | Even -> Format.fprintf fmt "even"
   | Odd -> Format.fprintf fmt "odd"
   | TOP -> Format.fprintf fmt "⊤"

   (* operator dispatch *)

  let unary x op = match op with
   | AST_UNARY_PLUS  ->  x
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
   | AST_UNARY_MINUS -> meet x r
 

  let bwd_binary x y op r = match op with
 
   | AST_PLUS ->
       meet x (sub r y), meet y (sub r x)
 
   | AST_MINUS ->
       meet x (add y r), meet y (sub x r)
         
   | AST_MULTIPLY ->
    let contains_zero o = subset (const Z.zero) o in
      (if contains_zero y && contains_zero r then x else meet x (div r y)),
      (if contains_zero x && contains_zero r then y else meet y (div r x))

   | AST_DIVIDE ->
       TOP,TOP


  let is_pair x = (x=Even)

  let fst _ = invalid_arg "first"
  let snd _ = invalid_arg "last"
 end : VALUE_DOMAIN)
