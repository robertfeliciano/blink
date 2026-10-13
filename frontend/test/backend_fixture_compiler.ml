module DA = Desugaring.Desugared_ast

let i32 = DA.TInt (DA.TSigned DA.Ti32)
let int value = DA.Int (string_of_int value, DA.TSigned DA.Ti32)
let optimization_level = Util.Optimization_level.O0

let function_ ?(args = []) ?(frtyp = DA.RetVal i32) name body =
  DA.{ annotations = []; frtyp; fname = name; args; body; inline = false }

let arithmetic () =
  let result = DA.Bop (DA.Add, int 20, int 22, i32) in
  DA.Prog
    ( optimization_level,
      [ function_ "main" [ DA.Ret (Some result) ] ],
      [],
      [],
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
  DA.Prog (optimization_level, [ double; main ], [], [], [], [], [])

let array_index () =
  let array_ty = DA.TRef (DA.RArray (i32, 3)) in
  let body =
    [
      DA.Decl
        ("values", array_ty, DA.Array ([ int 4; int 8; int 15 ], array_ty), true);
      DA.Ret (Some (DA.Index (DA.Id ("values", array_ty), int 2, i32)));
    ]
  in
  DA.Prog (optimization_level, [ function_ "main" body ], [], [], [], [], [])

let identity ty name =
  function_
    ~args:[ (ty, "values") ]
    ~frtyp:(DA.RetVal ty) name
    [ DA.Ret (Some (DA.Id ("values", ty))) ]

let array_call () =
  let array_ty = DA.TRef (DA.RArray (i32, 2)) in
  let matrix_ty = DA.TRef (DA.RArray (array_ty, 2)) in
  let body =
    [
      DA.Decl ("values", array_ty, DA.Array ([ int 20; int 22 ], array_ty), true);
      DA.Decl
        ( "direct",
          array_ty,
          DA.Call ("array_identity", [ DA.Id ("values", array_ty) ], array_ty),
          true );
      DA.Decl
        ( "matrix",
          matrix_ty,
          DA.Array
            ( [ DA.Id ("direct", array_ty); DA.Id ("values", array_ty) ],
              matrix_ty ),
          true );
      DA.Decl
        ( "nested",
          matrix_ty,
          DA.Call ("matrix_identity", [ DA.Id ("matrix", matrix_ty) ], matrix_ty),
          true );
      DA.Ret
        (Some
           (DA.Bop
              ( DA.Add,
                DA.Index (DA.Id ("direct", array_ty), int 0, i32),
                DA.Index
                  ( DA.Index (DA.Id ("nested", matrix_ty), int 1, array_ty),
                    int 1,
                    i32 ),
                i32 )));
    ]
  in
  DA.Prog
    ( optimization_level,
      [
        identity array_ty "array_identity";
        identity matrix_ty "matrix_identity";
        function_ "main" body;
      ],
      [],
      [],
      [],
      [],
      [] )

let array_callback () =
  let array_ty = DA.TRef (DA.RArray (i32, 2)) in
  let pointer_ty =
    DA.TRef (DA.RPtr (DA.TRef (DA.RFun ([ array_ty ], DA.RetVal array_ty))))
  in
  let apply =
    function_
      ~args:[ (pointer_ty, "callback"); (array_ty, "values") ]
      ~frtyp:(DA.RetVal array_ty) "apply"
      [
        DA.Ret
          (Some (DA.Call ("callback", [ DA.Id ("values", array_ty) ], array_ty)));
      ]
  in
  let body =
    [
      DA.Decl ("values", array_ty, DA.Array ([ int 20; int 22 ], array_ty), true);
      DA.Decl
        ( "result",
          array_ty,
          DA.Call
            ( "apply",
              [
                DA.Id ("array_identity", pointer_ty); DA.Id ("values", array_ty);
              ],
              array_ty ),
          true );
      DA.Ret
        (Some
           (DA.Bop
              ( DA.Add,
                DA.Index (DA.Id ("result", array_ty), int 0, i32),
                DA.Index (DA.Id ("result", array_ty), int 1, i32),
                i32 )));
    ]
  in
  DA.Prog
    ( optimization_level,
      [ identity array_ty "array_identity"; apply; function_ "main" body ],
      [],
      [],
      [],
      [],
      [] )

let dynamic_array_buffer () =
  let array_ty = DA.TRef (DA.RDArray i32) in
  let buffer = DA.Id ("buffer", array_ty) in
  let left = DA.Index (buffer, int 0, i32) in
  let right = DA.Index (buffer, int 1, i32) in
  let body =
    [
      DA.Decl ("buffer", array_ty, DA.ArrayAlloc (int 2, array_ty), true);
      DA.Assn (left, int 20, i32);
      DA.Assn (right, int 22, i32);
      DA.Decl ("result", i32, DA.Bop (DA.Add, left, right, i32), true);
      DA.Free [ buffer ];
      DA.Ret (Some (DA.Id ("result", i32)));
    ]
  in
  DA.Prog (optimization_level, [ function_ "main" body ], [], [], [], [], [])

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
  DA.Prog
    (optimization_level, [ function_ "main" body ], [ box ], [], [], [], [])

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
  DA.Prog (optimization_level, [ function_ "main" body ], [], [], [], [], [])

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
      [ global "count" i32 (int 20) ],
      [],
      [] )

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
      ],
      [],
      [] )

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
    ( optimization_level,
      [ function_ "main" checks ],
      [],
      [ strcmp ],
      globals,
      [],
      [] )

let interface_dispatch () =
  let box_ty = DA.TRef (DA.RClass "Box") in
  let iface_ty = DA.TRef (DA.RInterface "Value") in
  let box =
    DA.
      {
        cname = "Box";
        annotations = [];
        fields = [ { prelude = []; fieldName = "n"; ftyp = i32; init = int 0 } ];
      }
  in
  let receiver = DA.Id ("this", box_ty) in
  let get =
    function_
      ~args:[ (box_ty, "this"); (i32, "extra") ]
      "box_get"
      [
        DA.Ret
          (Some
             (DA.Bop
                (DA.Add, DA.Proj (receiver, "n", i32), DA.Id ("extra", i32), i32)));
      ]
  in
  let set =
    DA.
      {
        annotations = [];
        frtyp = RetVoid;
        fname = "box_set";
        args = [ (box_ty, "this"); (i32, "value") ];
        inline = false;
        body =
          [ Assn (Proj (receiver, "n", i32), Id ("value", i32), i32); Ret None ];
      }
  in
  let read =
    function_
      ~args:[ (iface_ty, "item") ]
      "read"
      [
        DA.InterfaceSCall (DA.Id ("item", iface_ty), 1, [ int 20 ], DA.RetVoid);
        DA.Ret
          (Some
             (DA.InterfaceCall (DA.Id ("item", iface_ty), 0, [ int 22 ], i32)));
      ]
  in
  let main =
    function_ "main"
      [
        DA.Decl ("box", box_ty, DA.ObjInit ("Box", []), false);
        DA.Decl
          ( "view",
            iface_ty,
            DA.InterfaceCast (DA.Id ("box", box_ty), "Box", "Value"),
            false );
        DA.Decl
          ( "result",
            i32,
            DA.Call ("read", [ DA.Id ("view", iface_ty) ], i32),
            false );
        DA.Free [ DA.Id ("view", iface_ty) ];
        DA.Ret (Some (DA.Id ("result", i32)));
      ]
  in
  let protos =
    [
      DA.{ annotations = []; frtyp = RetVal i32; fname = "get"; args = [ i32 ] };
      DA.{ annotations = []; frtyp = RetVoid; fname = "set"; args = [ i32 ] };
    ]
  in
  DA.Prog
    ( optimization_level,
      [ get; set; read; main ],
      [ box ],
      [],
      [],
      [ ("Value", protos) ],
      [ ("Box", "Value", [ "box_get"; "box_set" ]) ] )

let fixtures =
  [
    ("global-string-aliases", global_string_aliases);
    ("global-storage", global_storage);
    ("global-literals", global_literals);
    ("arithmetic", arithmetic);
    ("function-call", function_call);
    ("array-index", array_index);
    ("array-call", array_call);
    ("array-callback", array_callback);
    ("dynamic-array-buffer", dynamic_array_buffer);
    ("object-field", object_field);
    ("conditional", conditional);
    ("literal-values", literal_values);
    ("interface-dispatch", interface_dispatch);
  ]

let () =
  if Array.length Sys.argv <> 2 && Array.length Sys.argv <> 3 then (
    Printf.eprintf "usage: %s FIXTURE [-O0|-O2]\n" Sys.argv.(0);
    exit 2);
  match List.assoc_opt Sys.argv.(1) fixtures with
  | Some build ->
      let level =
        if Array.length Sys.argv = 2 then optimization_level
        else
          match Sys.argv.(2) with
          | "-O0" -> Util.Optimization_level.O0
          | "-O2" -> Util.Optimization_level.O2
          | option -> invalid_arg ("unknown optimization level: " ^ option)
      in
      let (DA.Prog (_, fs, cs, ps, gs, interfaces, impls)) = build () in
      DA.convert_caml_ast (DA.Prog (level, fs, cs, ps, gs, interfaces, impls))
  | None ->
      Printf.eprintf "unknown backend fixture: %s\n" Sys.argv.(1);
      exit 2
