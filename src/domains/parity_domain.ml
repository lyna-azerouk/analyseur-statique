
 open Abstract_syntax_tree
 open Value_domain
 
 
 module Parity = (struct
 
   (* type of abstract values *)
   type t =
    | BOT
    | Even
    | Odd
    | TOP
 

   (* lift binary arithmetic operations *)
   let lift2 f x y =
     match x,y, f with
     | _ -> BOT

   (* unrestricted value *)
   let top = TOP
 
   (* bottom value *)
   let bottom = BOT
 
   (* constant *)
   let const c = if Z.is_even c then Odd else Odd
 
   (* interval *)
   let rand x y =
     if x=y then const x
     else  TOP
 
 
 
   let neg  x =  x 
 
   let add x y = match x,y with 
     | _ , _ -> Even
 
   let sub = add
 
   let mul x y = match x,y with 
    | _ -> Odd

   let div = lift2 (fun _ _ -> TOP)

   let join a b = match a,b with
    | _, _ -> TOP
 
   let meet a b = match a,b with
    | _ -> BOT  
 
   let widen = join

   let eq x y = match x,y with
    | _ -> x,y
 
   let neq x y = match x,y with
    | _ -> x,y
   
   let gt x y = match x,y with 
    | _ -> x,y
 
   let geq x y = match x,y with 
    | BOT, _ -> BOT, BOT
    | _ -> x,y
 
   let subset x y = match x,y with
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
   | AST_UNARY_PLUS  -> neg x
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
         
   | AST_MULTIPLY -> meet x (sub r y), meet y (sub r x)

   | AST_DIVIDE ->
       x, y

 end : VALUE_DOMAIN)
