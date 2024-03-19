open Value_reduction
open Value_domain
open Abstract_syntax_tree

module ReducedProduct (R : VALUE_REDUCTION) = 
(struct

  module A = R.A
  module B = R.B

  type t = R.t (* A.t * B.t *)

  let top = ((A.top),(B.top))

  let bottom= ((A.bottom),(B.bottom))

  let const c = R.reduce ((A.const c),(B.const c))

  let rand x y =R.reduce ((A.rand x y),(B.rand x y))
	
  let join x y = R.reduce ((A.join (fst x) (fst y)),(B.join (snd x) (snd y)))

  let meet x y =  R.reduce ((A.meet (fst x) (fst y)),(B.meet (snd x) (snd y)))
 
  let widen x y= R.reduce ((A.widen (fst x) (fst y)),(B.widen (snd x) (snd y)))

  let subset x y =  ((A.subset (fst x) (fst y)) && (B.subset (snd x) (snd y))) 

  let is_bottom a= A.is_bottom (fst a) && B.is_bottom (snd a) 

  let print fmt ((x,y):t) =
    begin
      A.print fmt x;
      Format.fprintf fmt " ∧ ";
      B.print fmt y 
    end

  let unary ((a,b):t) (op:int_unary_op) : t =R.reduce ((A.unary a op),(B.unary b op))

  let binary x y op =R.reduce ((A.binary (fst x) (fst y) op),(B.binary (snd x) (snd y) op))

  let bwd_unary x op r =R.reduce ((A.bwd_unary (fst x) op (fst r) ),(B.bwd_unary (snd x) op (snd r)))

  let compare ((x1,y1):t) ((x2,y2):t) (op : compare_op) : t * t = 
      let a1, a2 = A.compare x1 x2 op
      and b1, b2 = B.compare y1 y2 op in 
      (R.reduce (a1,b1), R.reduce (a2,b2))

  let bwd_binary ((x1,y1):t) ((x2,y2):t) (op : int_binary_op) ((r1,r2):t): t * t = 
   let a1, a2 = A.bwd_binary x1 x2 op r1
    and b1, b2 = B.bwd_binary y1 y2 op r2 in 
    (R.reduce (a1,b1), R.reduce (a2,b2))
end : VALUE_DOMAIN)