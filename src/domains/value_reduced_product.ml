open Value_reduction
open Value_domain
open Abstract_syntax_tree

module ReducedProduct (R : VALUE_REDUCTION) = 
(struct

  module A = R.A
  module B = R.B

  type t = R.t (* A.t * B.t *)

  let top = A.top, B.top
  let bottom = A.bottom, B.bottom
  let const c = A.const c, B.const c
  let rand x y = A.rand x y, B.rand x y

  let lift f g ((x1,y1):t) ((x2,y2):t) =
     R.reduce (f x1 x2, g y1 y2)

  let join = lift A.join B.join
  let meet = lift A.meet B.meet 

  let subset ((x1,y1):t) ((x2,y2):t) = A.subset x1 x2 && B.subset y1 y2 

  let is_bottom = (=) bottom 
  let print fmt ((x,y):t) =
    begin
      A.print fmt x;
      Format.fprintf fmt " ∧ ";
      B.print fmt y 
    end

  let unary ((a, b) : t) (op : int_unary_op) : t =  match op with 
    | _ -> a,b 

  let binary ((x1,y1):t) ((x2,y2):t) (op : int_binary_op) : t = match op, x2,y2 with 
    | _ -> x1,y1

  let widen ((x1,y1):t) ((x2,y2):t) = 
    R.reduce (A.widen x1 x2, B.widen y1 y2)

  let compare ((x1,y1):t) ((x2,y2):t) (op: compare_op) : t * t =  match op with 
      | _ ->(x1,y1), (x2,y2)

  let bwd_unary ((x1,y1):t) (op : int_unary_op) ((x2,y2):t) : t =  match op, x2, y2 with 
  | _ , _,_->(x1,y1)

  let bwd_binary ((x1,y1):t) ((x2,y2):t) (op : int_binary_op) ((r1,r2):t): t * t =  match op, r1, r2 with 
  | _,_,_ -> (x1,y1), (x2,y2)


end : VALUE_DOMAIN)