open TokenTypes

let tokenize input =
  let re_lparen = Str.regexp "(" in
	let re_rparen = Str.regexp ")" in
	let re_lbrace = Str.regexp "{" in 
	let re_rbrace = Str.regexp "}" in
	let re_equal = Str.regexp "==" in 
	let re_notequal = Str.regexp "!=" in
	let re_assign = Str.regexp "=" in 
	let re_greater = Str.regexp ">" in
	let re_less = Str.regexp "<" in
	let re_greaterequal = Str.regexp ">=" in
  let re_lessequal = Str.regexp "<=" in
  let re_or = Str.regexp "||" in
	let re_and = Str.regexp "&&" in
	let re_not = Str.regexp "!" in
	let re_semi = Str.regexp ";" in
	let re_type_int = Str.regexp "int" in 
	let re_type_bool = Str.regexp "bool" in
	let re_print = Str.regexp "printf" in
	let re_main = Str.regexp "main" in
	let re_if = Str.regexp "if" in
  let re_else = Str.regexp "else" in
  let re_for = Str.regexp "for" in
  let re_from = Str.regexp "from" in
  let re_to = Str.regexp "to" in
  let re_while = Str.regexp "while" in
  let re_add = Str.regexp "+" in
	let re_sub = Str.regexp "-" in
	let re_mult = Str.regexp "*" in
	let re_div = Str.regexp "/" in
  let re_pow = Str.regexp "\\^" in
  let re_bool = Str.regexp "true\\|false" in
  let re_int = Str.regexp "-?[0-9]+" in
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let re_extra = Str.regexp "[a-zA-Z0-9]+" in
  let re_skip = Str.regexp "[ \t\n]*" in

  let rec next_token str pos = 
    if pos >= (String.length str) then [EOF]
    else if (Str.string_match re_lparen str pos) then (Tok_LParen)::(next_token str (pos+1))
    else if (Str.string_match re_rparen str pos) then (Tok_RParen)::(next_token str (pos+1))
    else if (Str.string_match re_lbrace str pos) then (Tok_LBrace)::(next_token str (pos+1))
    else if (Str.string_match re_rbrace str pos) then (Tok_RBrace)::(next_token str (pos+1))
    else if (Str.string_match re_equal str pos) then (Tok_Equal)::(next_token str (pos+2))
    else if (Str.string_match re_notequal str pos) then (Tok_NotEqual)::(next_token str (pos+2))
    else if (Str.string_match re_assign str pos) then (Tok_Assign)::(next_token str (pos+1))
    else if (Str.string_match re_greaterequal str pos) then (Tok_GreaterEqual)::(next_token str (pos+2))
    else if (Str.string_match re_lessequal str pos) then (Tok_LessEqual)::(next_token str (pos+2))
    else if (Str.string_match re_greater str pos) then (Tok_Greater)::(next_token str (pos+1))
    else if (Str.string_match re_less str pos) then (Tok_Less)::(next_token str (pos+1))
    else if (Str.string_match re_or str pos) then (Tok_Or)::(next_token str (pos+2))
    else if (Str.string_match re_and str pos) then (Tok_And)::(next_token str (pos+2))
    else if (Str.string_match re_not str pos) then (Tok_Not)::(next_token str (pos+1))
    else if (Str.string_match re_semi str pos) then (Tok_Semi)::(next_token str (pos+1))
    else if (Str.string_match re_type_int str pos) then 
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_Int_Type)::(next_token str new_pos) 
    else if (Str.string_match re_type_bool str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_Bool_Type)::(next_token str new_pos)
    else if (Str.string_match re_print str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_Print)::(next_token str new_pos)
    else if (Str.string_match re_main str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_Main)::(next_token str new_pos)
    else if (Str.string_match re_if str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_If)::(next_token str new_pos)
    else if (Str.string_match re_else str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_Else)::(next_token str new_pos)
    else if (Str.string_match re_for str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_For)::(next_token str new_pos)
    else if (Str.string_match re_from str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_From)::(next_token str new_pos)
    else if (Str.string_match re_to str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_To)::(next_token str new_pos)
    else if (Str.string_match re_while str pos) then
      let tok = Str.matched_string str in
      let new_pos = Str.match_end() in
      if (Str.string_match re_extra str new_pos) then
        let id = Str.matched_string str in
        (Tok_ID (tok^id))::(next_token str (Str.match_end()))
      else (Tok_While)::(next_token str new_pos)
    else if (Str.string_match re_bool str pos) then 
      let tok = Str.matched_string str in
      (Tok_Bool (bool_of_string tok))::(next_token str (Str.match_end()))
    else if (Str.string_match re_int str pos) then
      let tok = Str.matched_string str in
      (Tok_Int (int_of_string tok))::(next_token str (Str.match_end()))
    else if (Str.string_match re_id str pos) then
      let tok = Str.matched_string str in
      (Tok_ID (tok))::(next_token str (Str.match_end()))
    else if (Str.string_match re_add str pos) then (Tok_Add)::(next_token str (pos+1))
    else if (Str.string_match re_sub str pos) then (Tok_Sub)::(next_token str (pos+1))
    else if (Str.string_match re_mult str pos) then (Tok_Mult)::(next_token str (pos+1))
    else if (Str.string_match re_div str pos) then (Tok_Div)::(next_token str (pos+1))
    else if (Str.string_match re_pow str pos) then (Tok_Pow)::(next_token str (pos+1))
    else if (Str.string_match re_skip str pos) then (next_token str (Str.match_end()))
    else raise(InvalidInputException "Lexer Error")
  in
  next_token input 0
;;