{
open Parser

exception SyntaxError of string

let float_from_sci (input: string) : float = 
  let reg = Str.regexp "[eE]" in
  let parts = Str.split reg input in 
  match parts with 
  | [base; exp] -> (float_of_string base) *. (10. ** (float_of_string exp))
  | _ -> raise (SyntaxError (Printf.sprintf "Bad format for scientific notation: %s" input))

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* Regexes for tokens *)
let int = digit+
let decimal = digit+ '.' digit+
let scientific = digit+ ('E'|'e') '-'? digit+
let id = alpha (alpha | digit | '_')*
let generic_type_param = ['A'-'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let sint = 'i' ("8"|"16"|"32"|"64"|"128")
let uint = 'u' ("8"|"16"|"32"|"64"|"128")

rule read = parse 
  | whitespace { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | "." { DOT }
  | ".." { RANGE }
  | "..=" { RANGE_INCL }
  | ":" { COLON }
  | ";" { SEMI }
  | "=" { EQUAL }
  | "=>" { ARROW }
  | "->" { THIN_ARROW }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT } 
  | "/" { DIV }
  | "@" { AT }
  | "%" { MOD }
  | "**" { POW }
  | "<<" { LTLT }
  | ">>" { GTGT }
  | ">>>" { GTGTGT }
  | "+=" { PLUEQ }
  | "-=" { MINEQ }
  | "*=" { TIMEQ }
  | "/=" { DIVEQ }
  | "@=" { ATEQ }
  | "**=" { POWEQ }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "!=" { NEQ }
  | "==" { EQEQ }
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  | "not" { NOT }
  | "let" { LET }
  | "new" { NEW }
  | "const" { CONST }
  | "void" { TVOID }
  | "i8"   { Ti8 }
  | "i16"  { Ti16 }
  | "i32"  { Ti32 }
  | "i64"  { Ti64 }
  | "i128" { Ti128 }
  | "u8"   { Tu8 }
  | "u16"  { Tu16 }
  | "u32"  { Tu32 }
  | "u64"  { Tu64 }
  | "u128" { Tu128 }
  | "f32" { Tf32 }
  | "f64" { Tf64 }
  | "string" { TSTRING }
  | "bool" { TBOOL }
  | "null" { NULL }
  | "fun" { FUN }
  | "if" { IF }
  | "in" { IN }
  | "else" { ELSE }
  | "for" { FOR }
  | "by" { BY }
  | "while" { WHILE }
  | "break" { BREAK }
  | "continue" { CONT }
  | "return" { RETURN }
  | "true" { TRUE }
  | "false" { FALSE }
  (* | "where" { WHERE }
  | "import" { IMPORT }
  | "enable" { ENABLE } *)
  | "class" { CLASS }
  (* | "impls" { IMPLS } *)
  (* | "global" { GLOBAL } *)
  (* | "?" { QMARK } *)
  | "as" { AS }
  | "|"  { BAR } 
  | "fn" { FN }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf } 
  | "<" { LT_TYPE }
  | ">" { GT_TYPE }
  (* | "?" { OPT_TYPE } *)
  | id { IDENT (Lexing.lexeme lexbuf) }
  | int { INT (Z.of_string (Lexing.lexeme lexbuf)) }
  | decimal { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | scientific { FLOAT (float_from_sci (Lexing.lexeme lexbuf)) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | eof { EOF }
  | _ as c { raise (SyntaxError ("Unexpected character: " ^ (String.make 1 c))) }

and read_single_line_comment = parse 
  | newline { Lexing.new_line lexbuf ; read lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment = parse 
  | "*/" { read lexbuf }
  | newline { Lexing.new_line lexbuf ; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError "Lexing error: Unexpected EOF in comment.") }
  | _ { read_multi_line_comment lexbuf }

and read_string buf = parse 
  | '"' { STRING (Buffer.contents buf) }
  | '\\' { read_escaped_char lexbuf |> Buffer.add_char buf ; read_string buf lexbuf }
  | eof { raise (SyntaxError "Lexing error: Unexpected EOF in string.") }
  | _ { Lexing.lexeme lexbuf |> Buffer.add_string buf ; read_string buf lexbuf }

and read_escaped_char = parse 
  | 'n' { '\n' }
  | 'r' { '\r' }
  | 't' { '\t' }
  | '\\' { '\\' }
  | '"' { '"' }
  | '\'' { '\'' }
  | _ { raise (SyntaxError ("Lexing error: Unexpected escape used on char " ^ (Lexing.lexeme lexbuf))) }
