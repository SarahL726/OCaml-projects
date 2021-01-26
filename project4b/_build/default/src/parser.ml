open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_or toks

and parse_or toks = 
  let (toks_after_parse_and, expr) = parse_and toks in
  match (lookahead toks_after_parse_and) with
  | Tok_Or -> 
      let toks2 = match_token toks_after_parse_and Tok_Or in
      let (toks3, expr_after_parse_or) = parse_or toks2 in 
      (toks3, Or(expr, expr_after_parse_or))
  | _ -> (toks_after_parse_and, expr)

and parse_and toks = 
  let (toks_after_parse_equal, expr) = parse_equal toks in
  match (lookahead toks_after_parse_equal) with
  | Tok_And ->
      let toks2 = match_token toks_after_parse_equal Tok_And in
      let (toks3, expr_after_parse_and) = parse_and toks2 in
      (toks3, And(expr, expr_after_parse_and))
  | _ -> (toks_after_parse_equal, expr)

and parse_equal toks = 
  let (toks_after_parse_rela, expr) = parse_relational toks in
  match (lookahead toks_after_parse_rela) with
  | Tok_Equal -> 
      let toks2 = match_token toks_after_parse_rela Tok_Equal in
      let (toks3, expr_after_parse_equal) = parse_equal toks2 in
      (toks3, Equal(expr, expr_after_parse_equal))
  | Tok_NotEqual ->
      let toks2 = match_token toks_after_parse_rela Tok_NotEqual in
      let (toks3, expr_after_parse_notequal) = parse_equal toks2 in
      (toks3, NotEqual(expr, expr_after_parse_notequal))
  | _ -> (toks_after_parse_rela, expr)

and parse_relational toks = 
  let (toks_after_parse_add, expr) = parse_add toks in
  match (lookahead toks_after_parse_add) with
  | Tok_Greater -> 
      let toks2 = match_token toks_after_parse_add Tok_Greater in
      let (toks3, expr_after_parse_greater) = parse_relational toks2 in
      (toks3, Greater(expr, expr_after_parse_greater))
  | Tok_Less -> 
      let toks2 = match_token toks_after_parse_add Tok_Less in
      let (toks3, expr_after_parse_less) = parse_relational toks2 in
      (toks3, Less(expr, expr_after_parse_less))
  | Tok_GreaterEqual -> 
      let toks2 = match_token toks_after_parse_add Tok_GreaterEqual in
      let (toks3, expr_after_parse_greaterequal) = parse_relational toks2 in
      (toks3, GreaterEqual(expr, expr_after_parse_greaterequal))
  | Tok_LessEqual -> 
      let toks2 = match_token toks_after_parse_add Tok_LessEqual in
      let (toks3, expr_after_parse_lessequal) = parse_relational toks2 in
      (toks3, LessEqual(expr, expr_after_parse_lessequal))
  | _ -> (toks_after_parse_add, expr)

and parse_add toks = 
  let (toks_after_parse_mult, expr) = parse_mult toks in
  match (lookahead toks_after_parse_mult) with
  | Tok_Add -> 
      let toks2 = match_token toks_after_parse_mult Tok_Add in
      let (toks3, expr_after_parse_add) = parse_add toks2 in
      (toks3, Add(expr, expr_after_parse_add))
  | Tok_Sub -> 
      let toks2 = match_token toks_after_parse_mult Tok_Sub in
      let (toks3, expr_after_parse_sub) = parse_add toks2 in
      (toks3, Sub(expr, expr_after_parse_sub))
  | _ -> (toks_after_parse_mult, expr)

and parse_mult toks = 
  let (toks_after_parse_pow, expr) = parse_pow toks in
  match (lookahead toks_after_parse_pow) with
  | Tok_Mult ->
      let toks2 = match_token toks_after_parse_pow Tok_Mult in
      let (toks3, expr_after_parse_mult) = parse_mult toks2 in
      (toks3, Mult(expr, expr_after_parse_mult))
  | Tok_Div ->
      let toks2 = match_token toks_after_parse_pow Tok_Div in
      let (toks3, expr_after_parse_div) = parse_mult toks2 in
      (toks3, Div(expr, expr_after_parse_div))
  | _ -> (toks_after_parse_pow, expr)

and parse_pow toks = 
  let (toks_after_parse_unary, expr) = parse_unary toks in
  match (lookahead toks_after_parse_unary) with
  | Tok_Pow -> 
      let toks2 = match_token toks_after_parse_unary Tok_Pow in
      let (toks3, expr_after_parse_pow) = parse_pow toks2 in
      (toks3, Pow(expr, expr_after_parse_pow))
  | _ -> (toks_after_parse_unary, expr)

and parse_unary toks = 
  match (lookahead toks) with
  | Tok_Not -> 
      let toks2 = match_token toks Tok_Not in
      let (toks3, expr_after_parse_not) = parse_unary toks2 in
      (toks3, Not(expr_after_parse_not))
  | _ -> (parse_primary toks)

and parse_primary toks = 
  match (lookahead toks) with
  | Tok_Int(x) -> let toks2 = match_token toks (Tok_Int(x)) in (toks2, Int(x))
  | Tok_Bool(x) -> let toks2 = match_token toks (Tok_Bool(x)) in (toks2, Bool x)
  | Tok_ID(x) -> let toks2 = match_token toks (Tok_ID(x)) in (toks2, ID x)
  | Tok_LParen -> 
      (let toks2 = match_token toks Tok_LParen in
      let (toks3, expr_after_parse_LP) = parse_expr toks2 in
      match (lookahead toks3) with
      | Tok_RParen -> 
          (let toks4 = match_token toks3 Tok_RParen in
          (toks4, expr_after_parse_LP))
      | _ -> raise (InvalidInputException("Right paren not found")))
  | _ -> raise (InvalidInputException("Unsupported token"))

let rec parse_stmt toks : stmt_result =
  match (lookahead toks) with
  | Tok_Int_Type -> 
      let (toks2, dec) = declarestmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(dec, stmt))
  | Tok_Bool_Type ->
      let (toks2, dec) = declarestmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(dec, stmt))
  | Tok_ID(x) -> 
      let (toks2, ass) = assignstmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(ass, stmt))
  | Tok_Print -> 
      let (toks2, print) = printstmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(print, stmt))
  | Tok_If -> 
      let (toks2, if_stmt) = ifstmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(if_stmt, stmt))
  | Tok_For -> 
      let (toks2, for_stmt) = forstmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(for_stmt, stmt))
  | Tok_While -> 
      let (toks2, while_stmt) = whilestmt toks in
      let (toks3, stmt) = parse_stmt toks2 in
      (toks3, Seq(while_stmt, stmt))
  | _ -> (toks, NoOp)

and declarestmt toks = 
  match (lookahead toks) with
  | Tok_Int_Type ->
      (let toks2 = match_token toks Tok_Int_Type in
      match (lookahead toks2) with
      | Tok_ID(x) -> 
          (let toks3 = match_token toks2 (Tok_ID(x)) in
          let toks4 = match_token toks3 Tok_Semi in
          (toks4, Declare(Int_Type, x)))
      | _ -> raise (InvalidInputException("Declare error")))
  | Tok_Bool_Type ->
      (let toks2 = match_token toks Tok_Bool_Type in
      match (lookahead toks2) with
      | Tok_ID(x) ->
          (let toks3 = match_token toks2 (Tok_ID(x)) in
          let toks4 = match_token toks3 Tok_Semi in
          (toks4, Declare(Bool_Type, x)))
      | _ -> raise (InvalidInputException("Declare error")))
  | _ -> raise (InvalidInputException("Declare error"))

and assignstmt toks = 
  match (lookahead toks) with
  | Tok_ID(x) -> 
      let toks2 = match_token toks (Tok_ID(x)) in
      let toks3 = match_token toks2 Tok_Assign in
      let (toks4, expr_after_parse_expr) = parse_expr toks3 in
      let toks5 = match_token toks4 Tok_Semi in
      (toks5, Assign(x, expr_after_parse_expr))
  | _ -> raise (InvalidInputException("Assign error"))

and printstmt toks = 
  match (lookahead toks) with
  | Tok_Print -> 
      let toks2 = match_token toks Tok_Print in
      let toks3 = match_token toks2 Tok_LParen in
      let (toks4, expr_after_parse_expr) = parse_expr toks3 in
      let toks5 = match_token toks4 Tok_RParen in
      let toks6 = match_token toks5 Tok_Semi in
      (toks6, Print(expr_after_parse_expr))
  | _ -> raise (InvalidInputException("Print error"))

and ifstmt toks = 
  match (lookahead toks) with
  | Tok_If -> 
      (let toks2 = match_token toks Tok_If in
      let toks3 = match_token toks2 Tok_LParen in
      let (toks4, expr_after_parse_expr) = parse_expr toks3 in
      let toks5 = match_token toks4 Tok_RParen in
      let toks6 = match_token toks5 Tok_LBrace in
      let (toks7, if_stmt_after_parse_stmt) = parse_stmt toks6 in
      let toks8 = match_token toks7 Tok_RBrace in
      match (lookahead toks8) with
      | Tok_Else -> 
          (let toks9 = match_token toks8 Tok_Else in
          let toks10 = match_token toks9 Tok_LBrace in
          let (toks11, else_stmt_after_parse_stmt) = parse_stmt toks10 in
          let toks12 = match_token toks11 Tok_RBrace in
          (toks12, If(expr_after_parse_expr, if_stmt_after_parse_stmt, else_stmt_after_parse_stmt)))
      | _ -> (toks8, If(expr_after_parse_expr, if_stmt_after_parse_stmt, NoOp)))
  | _ -> raise(InvalidInputException("If error"))

and forstmt toks = 
  match (lookahead toks) with
  | Tok_For ->
      (let toks2 = match_token toks Tok_For in
      let toks3 = match_token toks2 Tok_LParen in
      match (lookahead toks3) with
      | Tok_ID(x) ->
          (let toks4 = match_token toks3 (Tok_ID(x)) in
          let toks5 = match_token toks4 Tok_From in
          let (toks6, from_expr_after_parse_expr) = parse_expr toks5 in
          let toks7 = match_token toks6 Tok_To in
          let (toks8, to_expr_after_parse_expr) = parse_expr toks7 in
          let toks9 = match_token toks8 Tok_RParen in
          let toks10 = match_token toks9 Tok_LBrace in
          let (toks11, stmt_after_parse_stmt) = parse_stmt toks10 in
          let toks12 = match_token toks11 Tok_RBrace in
          (toks12, For(x, from_expr_after_parse_expr, to_expr_after_parse_expr, stmt_after_parse_stmt)))
      | _ -> raise (InvalidInputException("For error1")))
  | _ -> raise (InvalidInputException("For error2"))
          

and whilestmt toks = 
  match (lookahead toks) with
  | Tok_While ->
      let toks2 = match_token toks Tok_While in
      let toks3 = match_token toks2 Tok_LParen in
      let (toks4, expr_after_parse_expr) = parse_expr toks3 in
      let toks5 = match_token toks4 Tok_RParen in
      let toks6 = match_token toks5 Tok_LBrace in
      let (toks7, stmt_after_parse_stmt) = parse_stmt toks6 in
      let toks8 = match_token toks7 Tok_RBrace in
      (toks8, While(expr_after_parse_expr, stmt_after_parse_stmt))
  | _ -> raise (InvalidInputException("While error"))

let parse_main toks : stmt =
  let toks2 = match_token toks Tok_Int_Type in
  let toks3 = match_token toks2 Tok_Main in
  let toks4 = match_token toks3 Tok_LParen in
  let toks5 = match_token toks4 Tok_RParen in
  let toks6 = match_token toks5 Tok_LBrace in
  let (toks7, stmt) = parse_stmt toks6 in
  let toks8 = match_token toks7 Tok_RBrace in
  match (lookahead toks8) with
  | EOF -> stmt
  | _ -> raise (InvalidInputException("No EOF"))
;;
