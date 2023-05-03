open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let rec tok pos str = 
    if pos >= String.length str then []
    else 
      (* Let token *)
      if Str.string_match (Str.regexp "let") str pos then 
        Tok_Let :: (tok (pos + 3) str)

      (* String Token *)
      else if Str.string_match (Str.regexp "\"[^\"]*\"") str pos then 
        let s = Str.matched_string str in 
          let token = String.sub s 1 ((String.length s) - 2) in 
            (Tok_String token) :: (tok (Str.match_end()) str)

      (* Boolean token *)
      else if Str.string_match (Str.regexp "true\\|false") str pos then 
        let token = Str.matched_string str in 
          (Tok_Bool (bool_of_string token)) :: (tok (Str.match_end()) str)

      (* Two cases of INT*)
      else if Str.string_match (Str.regexp "[0-9]+") str pos then 
        let token = Str.matched_string str in 
          (Tok_Int (int_of_string token)) :: (tok (Str.match_end()) str)
      else if Str.string_match (Str.regexp "(-[0-9]+)") str pos then 
        let s = Str.matched_string str in
        let token = String.sub s 1 ((String.length s) - 2) in 
          (Tok_Int (int_of_string token)) :: (tok (Str.match_end()) str)

      (* ARROW token *)
      else if Str.string_match (Str.regexp "->") str pos then 
      Tok_Arrow :: (tok (pos + 2) str)

      (* Left paren *)
      else if Str.string_match (Str.regexp "(") str pos then 
        Tok_LParen :: (tok (pos + 1) str)

      (* Right paren*)
      else if Str.string_match (Str.regexp ")") str pos then 
        Tok_RParen :: (tok (pos + 1 ) str)

      (* Equal *)
      else if Str.string_match (Str.regexp "=") str pos then 
        Tok_Equal :: (tok (pos + 1) str)
      
      (* Not equal *)
      else if Str.string_match (Str.regexp "<>") str pos then 
        Tok_NotEqual :: (tok (pos + 2) str)

      (* Greater than *)
      else if Str.string_match (Str.regexp ">") str pos then 
        Tok_Greater :: (tok (pos + 1) str) 

      (* Less than *)
      else if Str.string_match (Str.regexp "<") str pos then 
        Tok_Less :: (tok (pos + 1) str)

      (* Greater than or equal to *)
      else if Str.string_match (Str.regexp ">=") str pos then 
        Tok_GreaterEqual :: (tok (pos + 2) str)

      (* Less than or equal to *)
      else if Str.string_match (Str.regexp "<=") str pos then 
        Tok_LessEqual :: (tok (pos + 2) str)
      
      (* OR token *)
      else if Str.string_match (Str.regexp "||") str pos then 
        Tok_Or :: (tok (pos + 2) str) 
      
      (* AND token *)
      else if Str.string_match (Str.regexp "&&") str pos then 
        Tok_And :: (tok (pos + 2) str)

      (* NOT token *)
      else if Str.string_match (Str.regexp "not") str pos then 
        Tok_Not :: (tok (pos + 3) str) 

      (* IF token *)
      else if Str.string_match (Str.regexp "if") str pos then 
        Tok_If :: (tok (pos + 2) str)

      (* THEN token *)
      else if Str.string_match (Str.regexp "then") str pos then 
        Tok_Then :: (tok (pos + 4) str)

      (* ELSE token *)
      else if Str.string_match (Str.regexp "else") str pos then
        Tok_Else :: (tok (pos + 4) str)

      (* ADD token *)
      else if Str.string_match (Str.regexp "+") str pos then 
        Tok_Add :: (tok (pos + 1) str)
      
      (* SUB token *)
      else if Str.string_match (Str.regexp "-") str pos then 
        Tok_Sub :: (tok (pos + 1) str) 

      (* MULT token *)
      else if Str.string_match (Str.regexp "*") str pos then
        Tok_Mult :: (tok (pos + 1) str)

      (* DIV token *)
      else if Str.string_match (Str.regexp "/") str pos then 
        Tok_Div :: (tok (pos + 1) str)

      (* CONCAT token *)
      else if Str.string_match (Str.regexp "\\^") str pos then 
        Tok_Concat :: (tok (pos + 1) str)

      (* DEF token *)
      else if Str.string_match (Str.regexp "def") str pos then 
        Tok_Def :: (tok (pos + 3) str)

      (* IN token *)
      else if Str.string_match (Str.regexp "in") str pos then
        Tok_In :: (tok (pos + 2) str) 

      (* REC token *)
      else if Str.string_match (Str.regexp "rec") str pos then 
        Tok_Rec :: (tok (pos + 3) str) 
      
      (* FUN token *)
      else if Str.string_match (Str.regexp "fun") str pos then 
        Tok_Fun :: (tok (pos + 3) str)
      
      (* Double Semis *)
      else if Str.string_match (Str.regexp ";;") str pos then
        Tok_DoubleSemi :: (tok (pos + 2) str)
      
      (* ID token *)
      else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos then 
        let token = Str.matched_string str in 
          (Tok_ID token) :: (tok (Str.match_end()) str)
      
      (* Take care of whitespace *)
      else if Str.string_match (Str.regexp "[ \t\n]*") str pos then 
        tok (Str.match_end()) str

      else raise (InvalidInputException "Lexer")

  in tok 0 input
;;