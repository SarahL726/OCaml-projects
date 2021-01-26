open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a);;
    
let rec eval_expr env t = match t with
  | Int x -> Int_Val x
  | Bool x -> Bool_Val x
  | ID x -> (match List.assoc_opt x env with
    | Some v -> v
    | None -> raise (DeclareError "ID error"))
  | Add (e1,e2) -> 
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Int_Val (a+b)
      | _ -> raise (TypeError "Add error"))
  | Sub (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Int_Val (a-b)
      | _ -> raise (TypeError "Sub error"))
  | Mult (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Int_Val (a*b)
      | _ -> raise (TypeError "Mult error"))
  | Div (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> if b <> 0 then Int_Val (a/b) else raise (DivByZeroError)
      | _ -> raise (TypeError "Div error"))
  | Pow (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Int_Val (pow a b)
      | _ -> raise (TypeError "Pow error"))
  | Or (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Bool_Val a, Bool_Val b) -> Bool_Val (a||b)
      | _ -> raise (TypeError "Or error"))
  | And (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Bool_Val a, Bool_Val b) -> Bool_Val (a&&b)
      | _ -> raise (TypeError "And error"))
  | Not x -> (match eval_expr env x with
    | Bool_Val b -> Bool_Val (not b)
    | _ -> raise (TypeError "Not error"))
  | Greater (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a > b)
      | _ -> raise (TypeError "Greater error"))
  | Less (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a < b)
      | _ -> raise (TypeError "Less error"))
  | GreaterEqual (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a >= b)
      | _ -> raise (TypeError "GreaterEqual error"))
  | LessEqual (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a <= b)
      | _ -> raise (TypeError "LessEqual error"))
  | Equal (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a = b)
      | (Bool_Val a, Bool_Val b) -> Bool_Val (a = b)
      | _ -> raise (TypeError "Equal error"))
  | NotEqual (e1,e2) ->
      (let x = eval_expr env e1 in
      let y = eval_expr env e2 in
      match (x,y) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a <> b)
      | (Bool_Val a, Bool_Val b) -> Bool_Val (a <> b)
      | _ -> raise (TypeError "NotEqual error"))



let rec eval_stmt env s = match s with
  | NoOp -> env
  | Seq (s1,s2) ->
      (let env1 = eval_stmt env s1 in
      let env2 = eval_stmt env1 s2 in
      env2)
  | Declare (dt,s) ->
      (if not (List.exists (fun (x,y) -> x=s) env) then
      match dt with
      | Int_Type -> (s, Int_Val 0)::env
      | Bool_Type -> (s, Bool_Val false)::env
      else raise(DeclareError "Declare error"))
  | Assign (id,e) -> 
      (match (List.assoc_opt id env) with
      | Some (Int_Val x) -> (match eval_expr env e with
          | Int_Val v -> (id, Int_Val v)::env
          | _ -> raise (TypeError "Assign error"))
      | Some (Bool_Val x) -> (match eval_expr env e with
          | Bool_Val v -> (id, Bool_Val v)::env
          | _ -> raise (TypeError "Assign error"))
      | None -> raise (DeclareError "Assign error"))
  | If (e,s1,s2) ->
      (match eval_expr env e with
      | Bool_Val true -> eval_stmt env s1
      | Bool_Val false -> eval_stmt env s2
      | _ -> raise (TypeError "If error"))
  | While (e,s) ->
      (match eval_expr env e with
      | Bool_Val true -> eval_stmt (eval_stmt env s) (While (e,s))
      | Bool_Val false -> env
      | _ -> raise (TypeError "While error"))
  | For (str,e1,e2,s) ->
      (let r1 = eval_expr env e1 in
      let r2 = eval_expr env e2 in
      match (r1,r2) with
      | (Int_Val a, Int_Val b) ->
          if a <= b then
            let x1 = eval_stmt ((str,(Int_Val a))::env) s in
            let m = match List.assoc_opt str x1 with
            | Some (Int_Val v) -> Int_Val (v+1)
            | _ -> raise (TypeError "For error")
            in
            let x2 = (str,m)::x1 in eval_stmt x2 (
              For (str, (match List.assoc_opt str x2 with
              | Some(Int_Val v) -> Int v
              | _ -> raise (TypeError "For error")),
              Int b, s)
            )
          else env
      | _ -> raise (TypeError "For error"))
  | Print e ->
      (match eval_expr env e with
      | Bool_Val true -> let _ = print_output_bool true;print_output_string "\n" in env
      | Bool_Val false -> let _ = print_output_bool false;print_output_string "\n" in env
      | Int_Val x -> let _ = print_output_int x;print_output_string "\n" in env)


