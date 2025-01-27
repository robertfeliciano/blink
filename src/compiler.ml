open Parsing.Parse_blink

let compile ?(print_ast=false) ?(print_tast=false) lexbuf =
  
  parse_prog lexbuf |> 
  function 
  | Ok (_) -> Printf.printf "success! %b %b\n" print_ast print_tast
  | Error _ -> Printf.eprintf "unsuccess! %b %b\n" print_ast print_tast