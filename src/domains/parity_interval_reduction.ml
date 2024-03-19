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
          |_,true,true when A.top == par && a=b ->  A.even, itv
          |_,false,false when A.top == par && a=b ->  A.odd, itv
          | _, _, _  when A.top == par ->  par, itv
          | _, _, _  when A.bottom == par -> (A.bottom, B.bottom)
          | _ , _, _  when ((a = b) && (B.is_pair a)) ->  A.even, (B.interval a a)
          | _ , _, _  when ((a = b) && not(B.is_pair a)) ->  A.odd, (B.interval a a)
          | true, false, true  |  false, false, true ->par, (B.interval a2 b)
          | true, true, false | false, true, false->  if a >b2 then (A.bottom, B.bottom) else par, (B.interval a b2)
          | true, false, false -> if a2 >b2 then (A.bottom, B.bottom) else (if a2=b2 then A.odd, (B.interval a2 b2) else par,(B.interval a2 b2))
          | true, true, true | false, false, false -> par, itv
          | false, true, true -> if a2 >b2 then (A.bottom, B.bottom) else (if a2=b2 then A.even, (B.interval a b) else  par, (B.interval a2 b2))
        )))


end: VALUE_REDUCTION)