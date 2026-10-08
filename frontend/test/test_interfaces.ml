open OUnit2
module DA = Desugaring.Desugared_ast

let parse_exn source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = "interface-fixture.bl" };
  match Parsing.Parse.parse_prog lexbuf with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let type_exn source =
  match Typing.Type.type_prog (parse_exn source) with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let positive name source = name >:: fun _ -> ignore (type_exn source)

let negative ?(line = 1) name expected source =
  name >:: fun _ ->
  match Typing.Type.type_prog (parse_exn source) with
  | Error error ->
      let message = Core.Error.to_string_hum error in
      List.iter
        (fun substring ->
          assert_bool
            (Printf.sprintf "expected %S in diagnostic:\n%s" substring message)
            (Core.String.is_substring message ~substring))
        [
          "Type Error:";
          expected;
          Printf.sprintf "interface-fixture.bl:%d:" line;
        ]
  | Ok _ -> assert_failure "expected interface type checking to fail"

let contract = "interface I { fun value(input: i32) => i32; }\n"

let implementation =
  "class C impl I { fun value(other: i32) => i32 { return other; } }\n"

let suite =
  "interfaces"
  >::: [
         ( "exported interface parsing and ranges" >:: fun _ ->
           let (Ast.Prog (_, declarations)) =
             parse_exn "export interface I { fun value(input: i32) => i32; }"
           in
           match declarations with
           | [
            {
              elt =
                { declaration = Ast.Interface interface; export_loc = Some _ };
              loc;
            };
           ] ->
               assert_equal "I" interface.elt.iname;
               assert_equal 1 (List.length interface.elt.protos);
               assert_bool "interface range is preserved"
                 (loc <> Util.Range.norange);
               assert_bool "prototype range is preserved"
                 ((List.hd interface.elt.protos).loc <> Util.Range.norange)
           | _ -> assert_failure "expected one exported interface" );
         ( "interface bodies contain prototypes" >:: fun _ ->
           match
             Parsing.Parse.parse_prog
               (Lexing.from_string
                  "interface I { fun value() => i32 { return 1; } }")
           with
           | Error _ -> ()
           | Ok _ -> assert_failure "interface method bodies must be rejected"
         );
         positive "matching signatures ignore parameter names"
           (contract ^ implementation);
         positive "additional concrete methods are allowed"
           (contract
          ^ "class C impl I { fun value(x: i32) => i32 { return x; } fun \
             extra() => void {} }");
         positive "interface declarations may follow implementers"
           (implementation ^ contract);
         positive "class arguments convert to interface parameters"
           (contract ^ implementation
          ^ "fun read(x: I) => i32 { return x.value(42); } fun main() => i32 { \
             return read(new C {}); }");
         positive "interface methods can refer to interfaces"
           "interface I { fun other(value: I) => I; } class C impl I { fun \
            other(value: I) => I { return value; } }";
         negative ~line:2 "missing method" "missing interface method value"
           (contract ^ "class C impl I {}");
         negative ~line:2 "wrong parameter count" "signature does not match I"
           (contract ^ "class C impl I { fun value() => i32 { return 0; } }");
         negative ~line:2 "wrong parameter type" "signature does not match I"
           (contract
          ^ "class C impl I { fun value(x: i64) => i32 { return 0; } }");
         negative "parameter order matters" "signature does not match I"
           "interface I { fun value(a: i32, b: bool) => i32; } class C impl I \
            { fun value(a: bool, b: i32) => i32 { return b; } }";
         negative ~line:2 "wrong return type" "signature does not match I"
           (contract
          ^ "class C impl I { fun value(x: i32) => i64 { return 0; } }");
         negative "void return mismatch" "signature does not match I"
           "interface I { fun value() => void; } class C impl I { fun value() \
            => i32 { return 0; } }";
         negative "unknown interface" "Unknown interface Missing"
           "class C impl Missing {}";
         negative "class is not an interface" "Unknown interface I"
           "class I {} class C impl I {}";
         negative "duplicate interface" "Duplicate interface or class I"
           "interface I {} interface I {}";
         negative "duplicate interface method"
           "Duplicate interface method value"
           "interface I { fun value() => i32; fun value() => i32; }";
         negative "duplicate implementation" "Duplicate implemented interface I"
           "interface I {} class C impl I, I {}";
         negative "interfaces cannot be constructed"
           "Cannot construct interface"
           "interface I {} fun main() => i32 { let item = new I {}; return 0; }";
         negative ~line:2 "matching methods do not imply implementation"
           "Expected implementing class for interface I"
           (contract
          ^ "class C { fun value(x: i32) => i32 { return x; } } fun read(x: I) \
             => i32 { return x.value(1); } fun main() => i32 { return read(new \
             C {}); }");
         negative "different interface types are incompatible"
           "Expected implementing class for interface I"
           "interface I {} interface J {} class C impl I, J {} fun take(x: I) \
            => void {} fun bad(x: J) => void { take(x); }";
         negative "interface has no concrete fields"
           "Interface I has no member method field"
           "interface I {} fun read(x: I) => i32 { return x.field; }";
         negative "interface expression call needs all arguments"
           "invalid number of arguments supplied"
           "interface I { fun value(x: i32) => i32; } fun bad(item: I) => i32 \
            { return item.value(); }";
         negative "interface expression call rejects extra arguments"
           "invalid number of arguments supplied"
           "interface I { fun value(x: i32) => i32; } fun bad(item: I) => i32 \
            { return item.value(1, 2); }";
         negative "interface statement call needs all arguments"
           "invalid number of arguments supplied"
           "interface I { fun set(x: i32) => void; } fun bad(item: I) => void \
            { item.set(); }";
         negative "interface statement call rejects extra arguments"
           "invalid number of arguments supplied"
           "interface I { fun set(x: i32) => void; } fun bad(item: I) => void \
            { item.set(1, 2); }";
         negative "interface methods cannot be used as values"
           "Interface methods must be called directly"
           "interface I { fun value(x: i32) => i32; } fun bad(item: I) => void \
            { let method: (i32) -> i32 = item.value; }";
         negative "interface signature unknown type" "class undefined"
           "interface I { fun value(x: Missing) => i32; }";
         negative "array casts cannot change interface representation"
           "Cannot cast"
           "interface I {} class C impl I {} fun bad(items: [C; 1]) => [I; 1] \
            { return items as [I; 1]; }";
         negative "nested array casts cannot change interface representation"
           "Cannot cast"
           "interface I {} class C impl I {} fun bad(items: [[C; 1]; 1]) => \
            [[I; 1]; 1] { return items as [[I; 1]; 1]; }";
         negative "interface casts cannot recover an unchecked concrete class"
           "Cannot cast"
           "interface I {} class C impl I {} fun bad(item: I) => C { return \
            item as C; }";
         negative "function casts cannot change interface return representation"
           "Cannot cast"
           "interface I {} class C impl I {} fun bad(make: () -> C) => () -> I \
            { return make as (() -> I); }";
         ( "lowered interface tables, conversion, and call slots" >:: fun _ ->
           let typed =
             type_exn
               "interface I { fun value(x: i32) => i32; fun set(x: i32) => \
                void; } class C impl I { let n: i32 = 0; fun value(x: i32) => \
                i32 { return n + x; } fun set(x: i32) => void { n = x; } } fun \
                read(item: I) => i32 { item.set(20); return item.value(22); } \
                fun main() => i32 { return read(new C {}); }"
           in
           assert_bool "typed printer includes the interface contract"
             (Core.String.is_substring
                (Typing.Pprint_typed_ast.show_typed_program typed)
                ~substring:"interface I");
           match Desugaring.Desugar.desugar_prog typed with
           | Error error -> assert_failure (Core.Error.to_string_hum error)
           | Ok
               (DA.Prog
                  ( _,
                    functions,
                    _,
                    _,
                    [ ("I", protos) ],
                    [ ("C", "I", methods) ] ) as lowered) -> (
               assert_bool "lowered printer includes the implementation table"
                 (Core.String.is_substring
                    (Desugaring.Pprint_desugared_ast.show_desugared_program
                       lowered)
                    ~substring:"C impl I");
               assert_equal 2 (List.length protos);
               assert_equal 2 (List.length methods);
               let read =
                 List.find (fun (fn : DA.fdecl) -> fn.fname = "read") functions
               in
               assert_bool "void method uses slot one"
                 (List.exists
                    (function
                      | DA.InterfaceSCall
                          ( DA.Id (_, DA.TRef (DA.RInterface "I")),
                            1,
                            [ _ ],
                            DA.RetVoid ) ->
                          true
                      | _ -> false)
                    read.body);
               assert_bool "value method uses slot zero"
                 (List.exists
                    (function
                      | DA.Ret
                          (Some
                             (DA.InterfaceCall
                                ( DA.Id (_, DA.TRef (DA.RInterface "I")),
                                  0,
                                  [ _ ],
                                  _ ))) ->
                          true
                      | _ -> false)
                    read.body);
               let main =
                 List.find (fun (fn : DA.fdecl) -> fn.fname = "main") functions
               in
               match main.body with
               | [
                DA.Ret
                  (Some
                     (DA.Call
                        ( "read",
                          [ DA.InterfaceCast (DA.ObjInit ("C", _), "C", "I") ],
                          _ )));
               ] ->
                   ()
               | _ ->
                   assert_failure
                     "expected explicit concrete-to-interface conversion")
           | Ok _ ->
               assert_failure "expected interface and implementation metadata"
         );
       ]
