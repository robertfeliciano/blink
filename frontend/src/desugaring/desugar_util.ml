exception DesugarError of string

let desugar_error err =
  raise 
  (DesugarError err)