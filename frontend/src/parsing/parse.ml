open Core
open Lexer

let get_pos (pos : Lexing.position) =
  Fmt.str "line %d, col %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let parse_prog (lexbuf : Lexing.lexbuf) =
  try
    Ok (Parser.program Lexer.read lexbuf)
  with
  | SyntaxError msg ->
      let err = Fmt.str "Syntax Error at %s: %s" (get_pos lexbuf.lex_curr_p) msg in
      Error (Error.of_string err)
  | Parser.Error ->
      let err = Fmt.str "Parser Error at %s" (get_pos lexbuf.lex_curr_p) in
      Error (Error.of_string err)
  | Ast.ParserError (pos, msg) ->
    let err = Fmt.str "Parser Error at line %d, col %d: %s" 
                pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg in
    Error (Error.of_string err)
