open Parser

(* Evaluater *)

let rec eval (ast : expr) : int =
  match ast with
  | Int i -> i
  | Plus(e1,e2) -> 
      let n1 = eval e1 in
      let n2 = eval e2 in
      n1 + n2
  | Mult(e1,e2) ->
      let n1 = eval e1 in
      let n2 = eval e2 in
      n1 * n2

