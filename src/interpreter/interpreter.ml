(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)


(*
  Abstract interpreter by induction on the syntax.
  Parameterized by an abstract domain.
*)


open Abstract_syntax_tree
open Abstract_syntax_printer
open Domain


(* parameters *)
(* ********** *)


(* for debugging *)
let trace = ref false

let widening_delay = ref 0
let unroll = ref 0


(* utilities *)
(* ********* *)


(* print errors *)
let error ext s =
  Format.printf "%s: ERROR: %s@\n" (string_of_extent ext) s

let fatal_error ext s =
  Format.printf "%s: FATAL ERROR: %s@\n" (string_of_extent ext) s;
  exit 1



(* interpreter signature *)
(* ********************* *)


(* an interpreter only exports a single function, which does all the work *)
module type INTERPRETER =
sig
  (* analysis of a program, given its abstract syntax tree *)
  val eval_prog: prog -> unit
end



(* interpreter *)
(* *********** *)


(* the interpreter is parameterized by the choice of a domain D
   of signature Domain.DOMAIN
 *)

module Interprete(D : DOMAIN) =
(struct

  (* abstract element representing a set of environments;
     given by the abstract domain
   *)
  type t = D.t


  (* utility function to reduce the compexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax, and call the domain's function D.compare, to handle
     the arithmetic part

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
   *)
  let filter (a:t) (e:bool_expr ext) (r:bool) : t =

    (* recursive exploration of the expression *)
    let rec doit a (e,_) r = match e with

    (* boolean part, handled recursively *)
    | AST_bool_unary (AST_NOT, e) ->
        doit a e (not r)
    | AST_bool_binary (AST_AND, e1, e2) ->
        (if r then D.meet else D.join) (doit a e1 r) (doit a e2 r)
    | AST_bool_binary (AST_OR, e1, e2) ->
        (if r then D.join else D.meet) (doit a e1 r) (doit a e2 r)
    | AST_bool_const b ->
        if b = r then a else D.bottom ()

    (* arithmetic comparison part, handled by D *)
    | AST_compare (cmp, (e1,_), (e2,_)) ->
        (* utility function to negate the comparison, when r=false *)
        let inv = function
        | AST_EQUAL         -> AST_NOT_EQUAL
        | AST_NOT_EQUAL     -> AST_EQUAL
        | AST_LESS          -> AST_GREATER_EQUAL
        | AST_LESS_EQUAL    -> AST_GREATER
        | AST_GREATER       -> AST_LESS_EQUAL
        | AST_GREATER_EQUAL -> AST_LESS
        in
        let cmp = if r then cmp else inv cmp in
        D.compare a e1 cmp e2  (* call the comparinsion function in domain ==> !! define the function of compartion when e1 or e2 is a condatnt *)

    in
    doit a e r


  (* interprets a statement, by induction on the syntax *)
  let rec eval_stat (a:t) ((s,ext):stat ext) : t =
    let r = match s with

    | AST_block (decl,inst) ->
        (* add the local variables *)
        let a =
          List.fold_left
            (fun a ((_,v),_) -> D.add_var a v)
            a decl
        in
        (* interpret the block recursively *)
        let a = List.fold_left eval_stat a inst in
        (* destroy the local variables *)
        List.fold_left
          (fun a ((_,v),_) -> D.del_var a v)
          a decl

    | AST_assign ((i,_),(e,_)) ->
        (* assigment is delegated to the domain *)
        D.assign a i e

    | AST_if (e,s1,Some s2) ->

        let t = eval_stat (filter a e true ) s1 in
        let f = eval_stat (filter a e false) s2 in
        (* then join *)
        D.join t f

    | AST_if (e,s1,None) ->
        (* compute both branches *)
        let t = eval_stat (filter a e true ) s1 in
        let f = filter a e false in
        (* then join *)
        D.join t f

    (* Definition of the while-loop abstract syntax tree *)
| AST_while (e, s) ->
  (* Recursive function to iteratively apply transformations and widen until a fixed point or limit is reached *)
  let rec refine_until_stable (transform: t -> t) (current: t) (widen_limit: int) (unroll_limit: int): t =
    if unroll_limit > 0 then 
      let next = eval_stat (filter current e true) s in 
      refine_until_stable transform next widen_limit (unroll_limit - 1)
    else if widen_limit = 0 then 
      let transformed = transform current in 
      if D.subset transformed current then transformed
      else refine_until_stable transform (D.widen current transformed) 0 0 
    else
      let transformed = transform current in 
      if D.subset transformed current then transformed
      else refine_until_stable transform transformed (widen_limit - 1) 0
  in
    (* Transformation function that joins the abstract domain with the evaluation of the statement *)
    let transform_function x = D.join a (eval_stat (filter x e true) s) in

    (* Initialization and invocation of the refinement process *)
    let invariant = refine_until_stable transform_function a !widening_delay !unroll in
    
    (* Filtering the final invariant with the loop condition set to false *)
    filter invariant e false


    | AST_assert (e, _) ->
       let res = filter a (e, ext) false in
	      if not (D.is_bottom res) then
	        (error ext  "assertion failure");	
      filter a (e, ext) true

    | AST_print l ->
        (* print the current abstract environment *)
        let l' = List.map fst l in
        Format.printf "%s: %a@\n"
          (string_of_extent ext) (fun fmt v -> D.print fmt a v) l';
        (* then, return the original element unchanged *)
        a

    | AST_PRINT_ALL ->
        (* print the current abstract environment for all variables *)
        Format.printf "%s: %a@\n"
          (string_of_extent ext) D.print_all a;
        (* then, return the original element unchanged *)
        a

    | AST_HALT ->
        (* after halt, there are no more environments *)
        D.bottom ()

    in

    (* tracing, useful for debugging *)
    if !trace then
      Format.printf "stat trace: %s: %a@\n"
        (string_of_extent ext) D.print_all r;
    r


  (* entry-point of the program analysis *)
  let eval_prog (l:prog) : unit =
    (* simply analyze each statement in the program *)
    let _ = List.fold_left eval_stat (D.init()) l in
    (* nothing useful to return *)
    Format.printf "analysis ended@\n";
    ()


end : INTERPRETER)
