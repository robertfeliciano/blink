(* open Desugar_util
module Typed = Typing.Typed_ast

let desugar_program (prog : Typed.program) : Typed.program = 


let desugar_prog (prog : Typed.program) : (Typed.program, Core.Error.t) result = 
  try Ok (desugar_program prog)
  with DesugarError msg -> 
    let err = Fmt.str "Error: %s" msg in 
    Error (Core.Error.of_string err) *)