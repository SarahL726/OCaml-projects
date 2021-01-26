(* Type *)
type token =
| Tok_Int of int
| Tok_Mult
| Tok_Plus
| Tok_LParen
| Tok_RParen
| Tok_EOF

let string_of_token tok = match tok with
| Tok_Int(i) -> string_of_int i
| Tok_Mult -> "*"
| Tok_Plus -> "+"
| Tok_LParen -> "("
| Tok_RParen -> ")"
| Tok_EOF -> ""

let rec string_of_list conv lst = 
match lst with
| [] -> ""
| h::[] -> conv h
| h::t -> (conv h) ^ " " ^ (string_of_list conv t)

(* Given source code returns a token list. *)
let rec lexer (input : string) : token list =
  let length = String.length input in
  let rec helper pos = 
    if pos >= length then 
      [Tok_EOF]
    else if (Str.string_match (Str.regexp "(") input pos) then
      Tok_LParen::(helper (pos + 1))
    else if (Str.string_match (Str.regexp ")") input pos) then
      Tok_RParen::(helper (pos + 1))
    else if (Str.string_match (Str.regexp "\\*") input pos) then
      Tok_Mult::(helper (pos + 1))
    else if (Str.string_match (Str.regexp "\\+") input pos) then
      Tok_Plus::(helper (pos + 1))
    else if (Str.string_match (Str.regexp "-?[0-9]+") input pos) then
      let matched_int = Str.matched_string input in
      (Tok_Int (int_of_string matched_int))::(helper (pos + (String.length matched_int)))
    else helper(pos + 1)
  in
  helper 0
  
