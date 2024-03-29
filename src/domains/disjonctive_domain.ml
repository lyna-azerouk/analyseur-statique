open Abstract_syntax_tree
open Domain

module Disjonction(D:DOMAIN)  = (struct
  type t =
  | ENS of D.t
  | Union of t * t
  | BOT

  let init _= ENS(D.init ())

  let rec print formatter structure variables = match structure with
    | ENS element ->
      begin
        Format.fprintf formatter "{ ";
        D.print formatter element variables;  
        Format.fprintf formatter " }";
      end
    | Union(left, right) ->
      begin
        print formatter left variables;
        Format.fprintf formatter ", ";
        print formatter right variables;
      end
    | _ -> Format.fprintf formatter "BOT"


  let bottom _ = ENS(D.bottom ()) 

  let rec add_var a b = match a with  (* like the one in the: non relational domain*)
    | ENS x -> ENS(D.add_var x b)
    | Union(x, y) -> Union(add_var x b, add_var y b)
    |_ -> BOT

  let rec del_var (a:t) (b:var) : t = match a with
    | ENS x -> ENS(D.del_var x b)
    | Union(x, y) -> Union(del_var x b, del_var y b)
    |_ -> BOT

  let  assign a v expr  = match a with
      | ENS x -> ENS(D.assign x v expr)
      | _ -> BOT

  let rec is_bottom a  = match a with
    | ENS x -> D.is_bottom x
    | Union(x, y) -> is_bottom x && is_bottom y
    | _ -> false

  let join a b= Union(a, b)

  let rec meet (a: t) (b: t) : t =
    match a, b with
    | ENS x, ENS y -> ENS(D.meet x y)
    | ENS x, Union(y1, y2) 
    | Union(y1, y2), ENS x ->
        (match meet (ENS x) y1, meet (ENS x) y2 with
        | r, _ when is_bottom r -> meet (ENS x) y2
        | _, r when is_bottom r -> meet (ENS x) y1
        | r1, r2 -> Union(r1, r2))
    | Union(x1, x2), Union(y1, y2) ->
        (match meet x1 (Union(y1, y2)), meet x2 (Union(y1, y2)) with
        | r, _ when is_bottom r -> meet x2 (Union(y1, y2))
        | _, r when is_bottom r -> meet x1 (Union(y1, y2))
        | r1, r2 -> Union(r1, r2))
    | _ -> BOT  


    let widen (a:t) (b:t) : t = match a, b with 
    | _-> BOT
  
    let rec compare (a:t) (e1:int_expr) (op:compare_op) (e2:int_expr) : t =
      match a with
      | ENS x -> ENS(D.compare x e1 op e2)
      | Union(x, y) ->
          (match compare x e1 op e2, compare y e1 op e2 with
          | r1, r2 when is_bottom r1 && is_bottom r2 -> bottom ()
          | r1, r2 when is_bottom r1 -> r2
          | r1, r2 when is_bottom r2 -> r1
          | r1, r2 -> Union(r1, r2))
      | _ -> BOT
  

    let  subset (a:t) (b:t) : bool = match a, b with 
    | _ -> false


    let rec print_all formatter structure = match structure with
    | ENS element ->
      begin
        Format.fprintf formatter "{ ";
        D.print_all formatter element;  
        Format.fprintf formatter " }";
      end
    | Union(left, right) ->
      begin
        print_all formatter left;
        Format.fprintf formatter ", ";
        print_all formatter right;
      end
    | _ -> Format.fprintf formatter "BOT"

end :DOMAIN)