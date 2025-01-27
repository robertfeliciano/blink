open Core
open Parsing.Parse_blink


let compile ?(print_ast=false) ?(print_tast=false) (lexbuf: Lexing.lexbuf) =
  
  parse_prog lexbuf
  (* add rest of pipeline here later... *)
  |> 
  function 
  | Ok (_prog) -> printf "success! %b %b\n" print_ast print_tast
  | Error e -> eprintf "%s\n" (Error.to_string_hum e)