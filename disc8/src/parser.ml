open Lexer

(* Types *)
type expr =
| Int of int
| Plus of expr * expr
| Mult of expr * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (Failure(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = match toks with
	 h::t -> h
	| _ -> raise (Failure("Empty input to lookahead"))


exception InvalidInputException of string
(* Parses a token list. *)
let rec parser (toks : token list) : expr =
  let (remaining_tokens, expr) = parse_S toks in
  if remaining_tokens <> [Tok_EOF] then raise (InvalidInputException "oh no! still tokens left")
  else expr

(* Parses the S rule. *)
and parse_S (toks : token list) : (token list * expr) =
  let (toks_after_parse_M, expr) = parse_M toks in
  match (lookahead toks_after_parse_M) with
  | Tok_Plus -> 
      let toks2 = match_token toks_after_parse_M Tok_Plus in
      let (toks3, expr_after_parse_S) = parse_S toks2 in
      (toks3, Plus (expr, expr_after_parse_S))
  | _ -> (toks_after_parse_M, expr)


(* Parses the M rule. *)
and parse_M (toks : token list) : (token list * expr) =
  let (toks_after_parse_N, expr) = parse_N toks in
  match (lookahead toks_after_parse_N) with
  | Tok_Mult -> 
      let toks2 = match_token toks_after_parse_N Tok_Mult in
      let (toks3, expr_after_parse_M) = parse_M toks2 in
      (toks3, Mult (expr, expr_after_parse_M))
  | _ -> (toks_after_parse_N, expr)
            
(* Parses the N rule. *)
and parse_N (toks : token list) : (token list * expr) =
  match lookahead toks with
  | Tok_Int i -> let toks2 = match_token toks (Tok_Int i) in (toks2, Int i)
  | Tok_LParen -> 
      let toks2 = match_token toks (Tok_LParen) in
      let (toks3, expr) = parse_S toks2 in
      let toks4 = match_token toks3 Tok_RParen in
      (toks4, expr)
  | _ -> raise (InvalidInputException "Oh noes")


  let rec parse_S toks = 
    let t = lookahead toks in
    match t with
    | 'x' -> match_tok toks "x"
    | h::t -> match h with
        | 'y' -> match_
      

