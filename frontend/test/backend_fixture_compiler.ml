module DA = Desugaring.Desugared_ast

let i32 = DA.TInt (DA.TSigned DA.Ti32)
let int value = DA.Int (string_of_int value, DA.TSigned DA.Ti32)
let optimization_level = Util.Optimization_level.O0

let function_ ?(args = []) name body =
  DA.
    {
      annotations = [];
      frtyp = RetVal i32;
      fname = name;
      args;
      body;
      inline = false;
    }

let arithmetic () =
  let result = DA.Bop (DA.Add, int 20, int 22, i32) in
  DA.Prog
    ( optimization_level,
      [ function_ "main" [ DA.Ret (Some result) ] ],
      [],
      [],
      [] )

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
  DA.Prog (optimization_level, [ double; main ], [], [], [])

let array_index () =
  let array_ty = DA.TRef (DA.RArray (i32, 3)) in
  let body =
    [
      DA.Decl
        ("values", array_ty, DA.Array ([ int 4; int 8; int 15 ], array_ty), true);
      DA.Ret (Some (DA.Index (DA.Id ("values", array_ty), int 2, i32)));
    ]
  in
  DA.Prog (optimization_level, [ function_ "main" body ], [], [], [])

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
  DA.Prog (optimization_level, [ function_ "main" body ], [ box ], [], [])

let conditional () =
  let expression =
    DA.Conditional
      ( DA.Bool true,
        ([ DA.Decl ("chosen", i32, int 42, true) ], DA.Id ("chosen", i32)),
        ([ DA.Decl ("ignored", i32, int 99, true) ], DA.Id ("ignored", i32)),
        i32 )
  in
  DA.Prog
    ( optimization_level,
      [ function_ "main" [ DA.Ret (Some expression) ] ],
      [],
      [],
      [] )

let literal_values () =
  let body =
    [
      DA.Decl ("message", DA.TRef DA.RString, DA.Str "bridge", true);
      DA.Decl ("nothing", DA.TRef DA.RString, DA.Null (DA.TRef DA.RString), true);
      DA.Decl ("decimal", DA.TFloat DA.Tf64, DA.Float (42.0, DA.Tf64), true);
      DA.Ret
        (Some
           (DA.Uop
              ( DA.Neg,
                DA.Uop
                  ( DA.Neg,
                    DA.Cast (DA.Id ("decimal", DA.TFloat DA.Tf64), i32),
                    i32 ),
                i32 )));
    ]
  in
  DA.Prog (optimization_level, [ function_ "main" body ], [], [], [])

let global ?(constant = false) name ty initial =
  DA.{ gname = name; gtyp = ty; ginit = initial; gconst = constant }

let global_storage () =
  let count = DA.Id ("count", i32) in
  let bump =
    function_ "bump"
      [
        DA.Assn (count, DA.Bop (DA.Add, count, int 22, i32), i32);
        DA.Ret (Some count);
      ]
  in
  let shadow =
    function_ "shadow"
      [ DA.Decl ("count", i32, int 99, false); DA.Ret (Some count) ]
  in
  let main =
    function_ "main"
      [
        DA.Decl ("shadowed", i32, DA.Call ("shadow", [], i32), true);
        DA.If
          ( DA.Bop (DA.Neq, DA.Id ("shadowed", i32), int 99, DA.TBool),
            [ DA.Ret (Some (int 1)) ],
            [] );
        DA.Decl ("updated", i32, DA.Call ("bump", [], i32), true);
        DA.Ret (Some count);
      ]
  in
  DA.Prog
    ( optimization_level,
      [ bump; shadow; main ],
      [],
      [],
      [ global "count" i32 (int 20) ] )

let global_string_aliases () =
  let ty = DA.TRef DA.RString in
  let id name = DA.Id (name, ty) in
  let same left right = DA.Bop (DA.Eqeq, id left, id right, DA.TBool) in
  let main =
    function_ "main"
      [
        DA.If
          ( DA.Bop
              (DA.And, same "alias" "original", same "copy" "original", DA.TBool),
            [ DA.Ret (Some (int 42)) ],
            [ DA.Ret (Some (int 1)) ] );
      ]
  in
  DA.Prog
    ( optimization_level,
      [ main ],
      [],
      [],
      [
        global ~constant:true "alias" ty (id "original");
        global "copy" ty (id "alias");
        global ~constant:true "original" ty (DA.Str "shared");
      ] )

let global_literals () =
  let signed = DA.TSigned DA.Ti128 in
  let unsigned = DA.TUnsigned DA.Tu128 in
  let signed_ty = DA.TInt signed in
  let unsigned_ty = DA.TInt unsigned in
  let string_ty = DA.TRef DA.RString in
  let small_float_ty = DA.TFloat DA.Tf32 in
  let large_float_ty = DA.TFloat DA.Tf64 in
  let globals =
    [
      global ~constant:true "signed" signed_ty
        (DA.Int ("-170141183460469231731687303715884105728", signed));
      global ~constant:true "unsigned" unsigned_ty
        (DA.Int ("340282366920938463463374607431768211455", unsigned));
      global "flag" DA.TBool (DA.Bool true);
      global "small_float" small_float_ty (DA.Float (42.5, DA.Tf32));
      global "large_float" large_float_ty (DA.Float (-1.5, DA.Tf64));
      global ~constant:true "initial" string_ty (DA.Str "hello");
      global "message" string_ty (DA.Str "hello");
    ]
  in
  let mismatch name ty value =
    DA.Bop (DA.Neq, DA.Id (name, ty), value, DA.TBool)
  in
  let checks =
    [
      DA.If
        ( DA.Uop (DA.Not, DA.Id ("flag", DA.TBool), DA.TBool),
          [ DA.Ret (Some (int 1)) ],
          [] );
      DA.If
        ( mismatch "signed" signed_ty
            (DA.Int ("-170141183460469231731687303715884105728", signed)),
          [ DA.Ret (Some (int 2)) ],
          [] );
      DA.If
        ( mismatch "unsigned" unsigned_ty
            (DA.Int ("340282366920938463463374607431768211455", unsigned)),
          [ DA.Ret (Some (int 3)) ],
          [] );
      DA.If
        ( mismatch "large_float" large_float_ty (DA.Float (-1.5, DA.Tf64)),
          [ DA.Ret (Some (int 4)) ],
          [] );
      DA.Assn (DA.Id ("message", string_ty), DA.Str "world", string_ty);
      DA.If
        ( DA.Bop
            ( DA.Neq,
              DA.Call
                ("strcmp", [ DA.Id ("message", string_ty); DA.Str "world" ], i32),
              int 0,
              DA.TBool ),
          [ DA.Ret (Some (int 5)) ],
          [] );
      DA.If
        ( DA.Bop
            ( DA.Neq,
              DA.Call
                ("strcmp", [ DA.Id ("initial", string_ty); DA.Str "hello" ], i32),
              int 0,
              DA.TBool ),
          [ DA.Ret (Some (int 6)) ],
          [] );
      DA.Ret (Some (DA.Cast (DA.Id ("small_float", small_float_ty), i32)));
    ]
  in
  let strcmp =
    DA.
      {
        annotations = [ "C" ];
        frtyp = RetVal i32;
        fname = "strcmp";
        args = [ string_ty; string_ty ];
      }
  in
  DA.Prog
    (optimization_level, [ function_ "main" checks ], [], [ strcmp ], globals)

let fixtures =
  [
    ("global-string-aliases", global_string_aliases);
    ("global-storage", global_storage);
    ("global-literals", global_literals);
    ("arithmetic", arithmetic);
    ("function-call", function_call);
    ("array-index", array_index);
    ("object-field", object_field);
    ("conditional", conditional);
    ("literal-values", literal_values);
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
