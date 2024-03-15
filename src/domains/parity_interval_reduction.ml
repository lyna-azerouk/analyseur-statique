open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction : VALUE_REDUCTION = 
struct
  module A = Parity
  module B = Intervals
  
  type t = A.t * B.t


  let reduce ((p, i) : t) : t =
    match i, p with
    | _, _ -> (p, i)
end