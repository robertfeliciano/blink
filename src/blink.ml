let read_file (file:string) : string =
  let channel = open_in file in
  let lines = ref [] in
  try while true; do
      lines := input_line channel :: !lines
  done; ""
  with End_of_file ->
    close_in channel;
    String.concat "\n" (List.rev !lines)

let parse_oat_file filename =
  let lexbuf = read_file filename |> 
               Lexing.from_string
  in
  try
    Lexer.token lexbuf |> Parser.prog
  with
  | Parser.Error -> failwith @@ Printf.sprintf "Parse error at: %s"
      (Range.string_of_range (Range.lex_range lexbuf))

