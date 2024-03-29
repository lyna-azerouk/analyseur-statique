open Value_reduction
open Value_domain

module ReducedProduct (R : VALUE_REDUCTION) = 
(struct

  module A = R.A
  module B = R.B

  type t = R.t

  let top = ((A.top),(B.top))

  let bottom= ((A.bottom),(B.bottom))

  let is_bottom x= A.is_bottom (fst x) && B.is_bottom (snd x) 

  let const c = R.reduce ((A.const c),(B.const c))

  let rand x y =R.reduce ((A.rand x y),(B.rand x y))

  let join x y = R.reduce ((A.join (fst x) (fst y)),(B.join (snd x) (snd y)))

  let meet x y =  R.reduce ((A.meet (fst x) (fst y)),(B.meet (snd x) (snd y)))

  let subset x y =  ((A.subset (fst x) (fst y)) && (B.subset (snd x) (snd y))) 

  let unary ((x,y)) (op)  =R.reduce ((A.unary x op),(B.unary y op))

  let binary x y op =R.reduce ((A.binary (fst x) (fst y) op),(B.binary (snd x) (snd y) op))

  let bwd_unary x op r =R.reduce ((A.bwd_unary (fst x) op (fst r) ),(B.bwd_unary (snd x) op (snd r)))

  let compare (a,b) (c,d) op = let a1, a2 = A.compare a c op and b1, b2 = B.compare b d op in 
    (R.reduce (a1,b1), R.reduce (a2,b2))

  let bwd_binary (a,b) (c,d) op (r1,r2) =  let a1, a2 = A.bwd_binary a c op r1 and b1, b2 = B.bwd_binary b d op r2 in 
    (R.reduce (a1,b1), R.reduce (a2,b2))

  let widen x y= R.reduce ((A.widen (fst x) (fst y)),(B.widen (snd x) (snd y)))

  let print fmt (x,y) =
    begin
      A.print fmt x;
      Format.fprintf fmt " ∧ ";
      B.print fmt y 
    end

end : VALUE_DOMAIN)