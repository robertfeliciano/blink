
let parse_prog lexbuf = 
  Ok (Parser.prog Lexer.read lexbuf) 