open Arg

module Range = Util.Range

let read_file (file:string) : string =
  let channel = open_in file in
  let lines = ref [] in
  try while true; do
      lines := input_line channel :: !lines
  done; ""
  with End_of_file ->
    close_in channel;
    String.concat "\n" (List.rev !lines)

let parse filename =
  let lexbuf = read_file filename |> Lexing.from_string in
  try
    Parser.prog Lexer.read lexbuf
  with
  | Parser.Error -> 
    failwith @@ Printf.sprintf "Parse error at: %s"
      (Range.string_of_range (Range.lex_range lexbuf))

let print_ast_flag = ref false

let args = 
  [ ("--print-ast", Set print_ast_flag, "prints the ast of the program")]

let files = ref []

let () = 
  try 
    (* let args = Array.to_list Sys.argv in 
    let filename = 
    match args with
      | [] | [_] -> (* This case is impossible because Sys.argv always has at least one element *)
        failwith "No arguments provided."
      | _ :: file :: [] -> 
        file
      | _ -> 
        failwith "Multiple files not supported yet!"
    in  *)
    Arg.parse args (fun filename -> files := filename :: !files) "placeholder";
    let ast = parse @@ List.hd !files in 
    begin
    Printf.printf("Successful parse\n");
    if !print_ast_flag then 
      Astlib.print_ast ast
    end
  with 
  | Failure msg -> Printf.eprintf "Error: %s\n" msg
  | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e)