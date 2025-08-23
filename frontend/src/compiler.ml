open Core
open Parsing.Parse_blink
open Ast


let compile ?(print_ast=false) (lexbuf: Lexing.lexbuf) =
  
  parse_prog lexbuf
  (* add rest of pipeline here later... *)
  |> 
  function 
  | Ok (prog) -> 
    if print_ast then printf "%s\n" (show_prog prog) else printf "Done!\n" ;
    (* convert_caml_ast prog *)
  | Error e -> eprintf "%s\n" (Error.to_string_hum e)