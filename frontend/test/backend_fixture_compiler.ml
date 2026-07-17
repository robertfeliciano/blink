module DA = Desugaring.Desugared_ast

let i32 = DA.TInt (DA.TSigned DA.Ti32)
let int value = DA.Int (string_of_int value, DA.TSigned DA.Ti32)

let function_ ?(args = []) name body =
  DA.{ annotations = []; frtyp = RetVal i32; fname = name; args; body }

let arithmetic () =
  let result = DA.Bop (DA.Add, int 20, int 22, i32) in
  DA.Prog ([ function_ "main" [ DA.Ret (Some result) ] ], [], [])

let function_call () =
  let double =
    function_
      ~args:[ (i32, "value") ]
      "double"
      [ DA.Ret (Some (DA.Bop (DA.Mul, DA.Id ("value", i32), int 2, i32))) ]
  in
  let main =
    function_ "main" [ DA.Ret (Some (DA.Call ("double", [ int 21 ], i32))) ]
  in
  DA.Prog ([ double; main ], [], [])

let array_index () =
  let array_ty = DA.TRef (DA.RArray (i32, 3)) in
  let body =
    [
      DA.Decl
        ("values", array_ty, DA.Array ([ int 4; int 8; int 15 ], array_ty), true);
      DA.Ret (Some (DA.Index (DA.Id ("values", array_ty), int 2, i32)));
    ]
  in
  DA.Prog ([ function_ "main" body ], [], [])

let object_field () =
  let field name =
    DA.{ prelude = []; fieldName = name; ftyp = i32; init = int 0 }
  in
  let box =
    DA.
      {
        cname = "Box";
        fields = [ field "left"; field "right" ];
        annotations = [];
      }
  in
  let box_ty = DA.TRef (DA.RClass "Box") in
  let body =
    [
      DA.Decl
        ( "box",
          box_ty,
          DA.ObjInit ("Box", [ ("left", int 20); ("right", int 24) ]),
          false );
      DA.Ret
        (Some
           (DA.Bop
              ( DA.Add,
                DA.Proj (DA.Id ("box", box_ty), "left", i32),
                DA.Proj (DA.Id ("box", box_ty), "right", i32),
                i32 )));
    ]
  in
  DA.Prog ([ function_ "main" body ], [ box ], [])

let fixtures =
  [
    ("arithmetic", arithmetic);
    ("function-call", function_call);
    ("array-index", array_index);
    ("object-field", object_field);
  ]

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "usage: %s FIXTURE\n" Sys.argv.(0);
    exit 2);
  match List.assoc_opt Sys.argv.(1) fixtures with
  | Some build -> DA.convert_caml_ast (build ())
  | None ->
      Printf.eprintf "unknown backend fixture: %s\n" Sys.argv.(1);
      exit 2
