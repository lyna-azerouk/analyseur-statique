open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction : VALUE_REDUCTION = 
(struct
module A = Parity
module B = Intervals

type t = A.t * B.t

(* let pair x = (Z.erem x (Z.of_int 2)) = Z.zero *)

let reduce ((par, itv):t) = 
  (if itv = B.bottom then ((A.bottom, B.bottom))
  else
    (let a = B.fst itv and b = B.snd itv in
    let a2 = (B.plus_one a) and b2 = (B.minus_one b) in
        (match A.is_pair par, B.is_pair a, B.is_pair b with
          | _, _, _  when A.top == par ->  par, itv
          | _, _, _  when A.bottom == par -> (A.bottom, B.bottom)
          | true, true, false -> if a2 >b then (A.bottom, B.bottom) else  par, (B.interval a2 b)
          | true, false, true ->  if a >b2 then (A.bottom, B.bottom) else par, (B.interval a b2)
          | true, false, false -> if a >b then (A.bottom, B.bottom) else  par, (B.interval a b)
          | true, true, true -> if a >b then (A.bottom, B.bottom) else par, (B.interval a b)
          | false, false, false -> if a >b then (A.bottom, B.bottom) else  par, (B.interval a b)
          | false, true, true ->  if a2 >b2 then (A.bottom, B.bottom) else par, (B.interval a2 b2)
          | false, false, true ->  if a >b2 then (A.bottom, B.bottom) else par, (B.interval a b2)
          | false, true, false -> if a2 >b then (A.bottom, B.bottom) else par, (B.interval a2 b)
        )))

  (* let parity (x ) = match x with 
      |  v -> A.const v 

  let reduce ((p, itv):t) =
    (if itv = B.bottom then (A.bottom, B.bottom)
    else
        (let a' = if A.subset (parity (B.fst itv)) p then (B.fst itv) else  (Z.add (B.fst itv) (Z.of_int 1)) in
        let b' = if A.subset (parity (B.snd itv)) p then (B.snd itv) else  (Z.sub (B.snd itv) (Z.of_int 1)) in
        if a' = b' then (parity a', (B.rand a' b'))
        else (p, (B.rand a'  b')))
        ) *)



end: VALUE_REDUCTION)