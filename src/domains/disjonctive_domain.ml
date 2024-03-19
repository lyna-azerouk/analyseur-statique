open Abstract_syntax_tree
open Domain

module Disjunctions(D:DOMAIN) = (struct         (* le domaine D est non-relationnal*)

  type t =
  | SIMPLE of D.t
  | DISJ of t * t
  | BOT
  | TOP

  let bottom (_) = SIMPLE(D.bottom ())

  let rec print (fmt:Format.formatter) (a:t) (l:var list) : unit = match a with (* if a var is in varlist, then we print it*)
    | SIMPLE x ->
      (Format.fprintf fmt "{ ";
      D.print fmt x l;
      Format.fprintf fmt " }";)
    | DISJ(x, y) ->
      (print fmt x l;
      Format.fprintf fmt ", ";
      print fmt y l; )
    | BOT -> Format.fprintf fmt "BOT"
    | TOP -> Format.fprintf fmt "TOP"
  
  let const (c:constant) : t = match c with
    | Cst x -> SIMPLE (D.const x)
    | Bot -> BOT
    | Top -> TOP
end: DOMAIN)