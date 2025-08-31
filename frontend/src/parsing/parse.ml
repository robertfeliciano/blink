open Core
open Lexer 

let get_pos (lexbuf: Lexing.lexbuf) = 
  let pos = lexbuf.lex_curr_p in
  Fmt.str "line %d, col %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let parse_prog (lexbuf: Lexing.lexbuf) = 
  try Ok (Parser.program Lexer.read lexbuf) with 
  | SyntaxError msg -> 
    let err = Fmt.str "Syntax Error at %s: %s" (get_pos lexbuf) msg in 
    Error (Error.of_string err)
  | Parser.Error -> 
    let err = Fmt.str "Parser Error at %s" (get_pos lexbuf) in
    Error (Error.of_string err)