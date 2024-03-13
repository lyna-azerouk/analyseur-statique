(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)


module ConcreteAnalysis =
  Interpreter.Interprete(Concrete_domain.Concrete)

module ConstantAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Constant_domain.Constants))

module IntervalAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Interval_domain.Intervals))

module Parity =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
      (Parity_domain.Parity))

    
module ParityIntervalAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
      (Value_reduced_product.ReducedProduct(
        Parity_interval_reduction.ParityIntervalsReduction
      )))
(* parse and print filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  Abstract_syntax_printer.print_prog Format.std_formatter prog


(* default action: print back the source *)
let eval_prog prog =
  Abstract_syntax_printer.print_prog Format.std_formatter prog

(* entry point *)
let main () =
  let action = ref eval_prog in
  let files = ref [] in
  (* parse arguments *)
  Arg.parse
    (* handle options *)
    ["-trace",
     Arg.Set Interpreter.trace,
     "Show the analyzer state after each statement";

     "-nonreldebug",
     Arg.Set Non_relational_domain.non_relational_debug,
     "Turns on debugging information for the non relational lifter";

     "-concrete",
     Arg.Unit (fun () -> action := ConcreteAnalysis.eval_prog),
     "Use the concrete domain";

     "-constant",
     Arg.Unit (fun () -> action := ConstantAnalysis.eval_prog),
     "Use the constant abstract domain";

     (* options to add *)
     (* -interval *)
	   "-interval", Arg.Unit (fun ()  -> action := IntervalAnalysis.eval_prog),"";
     "-delay",     Arg.Set_int Interpreter.widening_delay, "";
     (* -unroll *)
     (*-parity-interval *)
     "-parity-interval", Arg.Unit (fun () -> action := ParityIntervalAnalysis.eval_prog),"";
     ]
    (* handle filenames *)

    
    (* handle filenames *)
    (fun filename -> files := (!files)@[filename])
    "";
  List.iter
    (fun filename ->
      let prog = File_parser.parse_file filename in
      !action prog
    )
    !files

let _ = main ()
