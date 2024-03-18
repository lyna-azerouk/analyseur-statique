open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction : VALUE_REDUCTION = 
(struct
module A = Parity
module B = Intervals

type t = A.t * B.t

(* let pair x = (Z.erem x (Z.of_int 2)) = Z.zero *)

(* let reduce ((par, itv):t) = 
  (if itv = B.bottom then  (print_endline("Bottom"); (A.bottom, B.bottom))
  else
      (let a = B.fst itv and b = B.snd itv in
      let a2 = Z.add a (Z.of_int 1) and b2 = Z.sub b (Z.of_int 1) in
        (match A.is_pair par, pair a, pair b with
          | false, false, false-> par, itv
          | false, true, true  -> par, (B.rand a b)
          | _,_,_ -> par, itv
      ))) *)

  let parity (x ) = match x with 
      |  v -> A.const v 

  let reduce ((p, itv):t) =
    (if itv = B.bottom then (A.bottom, B.bottom)
    else
        (let a' = if A.subset (parity (B.fst itv)) p then (B.fst itv) else  (Z.add (B.fst itv) (Z.of_int 1)) in
        let b' = if A.subset (parity (B.snd itv)) p then (B.snd itv) else  (Z.sub (B.snd itv) (Z.of_int 1)) in
        if a' = b' then (parity a', (B.rand a' b'))
        else (p, (B.rand a'  b')))
        )



end: VALUE_REDUCTION)