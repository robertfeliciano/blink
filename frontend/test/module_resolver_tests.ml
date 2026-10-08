open OUnit2
open Module_test_support
open Ast

let helper = ([ "helper" ], "export fun answer() => i32 { return 42; }")
let main body = ([ "main" ], "import helper; fun main() => i32 { " ^ body ^ " }")

let rejects name substring sources =
  name >:: fun _ -> assert_resolution_error ~substring sources

let types name sources = name >:: fun _ -> assert_types sources

let suite =
  "module resolver"
  >::: [
         types "exported interfaces and qualified implementation"
           [
             ( [ "contracts" ],
               "export interface I { fun value() => i32; } export fun read(x: \
                I) => i32 { return x.value(); }" );
             ( [ "main" ],
               "import contracts; class C impl contracts.I { fun value() => \
                i32 { return 42; } } fun main() => i32 { let x: contracts.I = \
                new C {}; return contracts.read(x); }" );
           ];
         types "same interface spelling in distinct modules"
           [
             ([ "left" ], "export interface I { fun value() => i32; }");
             ([ "right" ], "export interface I { fun value() => i32; }");
             ( [ "main" ],
               "import left; import right; class C impl left.I, right.I { fun \
                value() => i32 { return 42; } } fun main() => i32 { let l: \
                left.I = new C {}; let r: right.I = new C {}; return \
                l.value(); }" );
           ];
         rejects "private interface cannot be implemented" "is private"
           [
             ([ "contracts" ], "interface I { fun value() => i32; }");
             ( [ "main" ],
               "import contracts; class C impl contracts.I { fun value() => \
                i32 { return 42; } } fun main() => i32 { return 0; }" );
           ];
         rejects "private interface cannot be annotated" "is private"
           [
             ([ "contracts" ], "interface I { fun value() => i32; }");
             ( [ "main" ],
               "import contracts; fun read(x: contracts.I) => i32 { return \
                x.value(); } fun main() => i32 { return 0; }" );
           ];
         ( "symbol encoding" >:: fun _ ->
           let encode owner name =
             match Modules.Module_symbols.encode ~owner ~name with
             | Ok value -> value
             | Error error -> assert_failure error.message
           in
           let left = encode [ "a"; "bc" ] "Box" in
           let right = encode [ "ab"; "c" ] "Box" in
           assert_bool "unambiguous components" (left <> right);
           assert_bool "unambiguous ownership"
             (encode [ "a" ] "bc" <> encode [ "a"; "bc" ] "c");
           assert_equal "a.bc.Box" (Modules.Module_symbols.display_names left);
           assert_equal "ab.c.Box" (Modules.Module_symbols.display_names right);
           assert_equal "_BLM999_"
             (Modules.Module_symbols.display_names "_BLM999_") );
         ( "resolved boundary" >:: fun _ ->
           let (Prog (imports, items)) =
             resolved [ helper; main "return helper.answer();" ]
           in
           assert_equal [] imports;
           assert_bool "exports consumed"
             (List.for_all (fun item -> item.elt.export_loc = None) items);
           let functions, _, _ =
             partition_declarations (Prog (imports, items))
           in
           assert_equal
             [ "helper.answer"; "main" ]
             (List.map
                (fun (fn : fdecl node) ->
                  Modules.Module_symbols.display_names fn.elt.fname)
                functions);
           assert_equal "helper.bl"
             (let file, _, _ = (List.hd functions).loc in
              file) );
         types "private local helper"
           [
             ( [ "helper" ],
               "fun secret() => i32 { return 42; } export fun answer() => i32 \
                { return secret(); }" );
             main "return helper.answer();";
           ];
         types "explicit nested alias"
           [
             ([ "app"; "geometry" ], "export class Box { let value: i32; }");
             ( [ "main" ],
               "import app.geometry as shapes; fun main() => i32 { let b: \
                shapes.Box = new shapes.Box { value = 42 }; return b.value; }"
             );
           ];
         types "same spellings and nominal signatures"
           [
             ( [ "left" ],
               "export class Box { let value: i32; } export fun read(b: Box) \
                => i32 { return b.value; }" );
             ( [ "right" ],
               "export class Box { let value: i32; } export fun read(b: Box) \
                => i32 { return b.value; }" );
             ( [ "main" ],
               "import left; import right; fun main() => i32 { let a = new \
                left.Box { value = 20 }; let b = new right.Box { value = 22 }; \
                return left.read(a) + right.read(b); }" );
           ];
         types "shadowing after initializer"
           [
             helper;
             ( [ "main" ],
               "import helper; fun value() => i32 { return 42; } fun main() => \
                i32 { let value = value(); return value; }" );
           ];
         types "nested scope does not leak"
           [
             helper;
             ( [ "main" ],
               "import helper; fun value() => i32 { return 42; } fun main() => \
                i32 { if true { let value = 1; } return value(); }" );
           ];
         types "captures and lambda shadowing"
           [
             helper;
             main
               "let answer = 2; let f: (i32) -> i32 = fn[answer](value) { \
                return answer + helper.answer() - value; }; return f(2);";
           ];
         types "captured module function"
           [
             helper;
             ( [ "main" ],
               "import helper; fun answer() => i32 { return 42; } fun main() \
                => i32 { let f: () -> i32 = fn[answer]() { return answer(); }; \
                return f(); }" );
           ];
         types "class defaults and constructors"
           [
             ( [ "helper" ],
               "fun seed() => i32 { return 42; } export class Box { let value: \
                i32 = seed(); fun Box() => Box { return new Box {}; } fun \
                read() => i32 { return this.value; } }" );
             main "let b: helper.Box; return b.read();";
           ];
         types "qualified array and function types casts ternaries"
           [
             ( [ "helper" ],
               "export class Box { let value: i32; } export fun read(b: Box) \
                => i32 { return b.value; }" );
             main
               "let boxes: [helper.Box; 1] = [new helper.Box { value = 42 }]; \
                let f: (helper.Box) -> i32 = helper.read; return true ? \
                f(boxes[0] as helper.Box) : 0;";
           ];
         types "exported prototype plus definition"
           [
             ( [ "helper" ],
               "export fun answer() => i32; fun answer() => i32 { return 42; }"
             );
             main "return helper.answer();";
           ];
         types "C duplicate signatures"
           [
             ( [ "left" ],
               "@C fun puts(text: string) => i32; export fun say() => i32 { \
                return puts(\"left\"); }" );
             ([ "right" ], "export @C fun puts(other: string) => i32;");
             ( [ "main" ],
               "import left; import right; fun main() => i32 { \
                right.puts(\"right\"); return 42; }" );
           ];
         types "shared nominal C signature through distinct aliases"
           [
             ([ "geometry" ], "export class Box { let value: i32 = 0; }");
             ( [ "left" ],
               "import geometry as g; @C fun consume(box: g.Box) => i32;" );
             ( [ "right" ],
               "import geometry as other; @C fun consume(value: other.Box) => \
                i32;" );
             ( [ "main" ],
               "import left; import right; fun main() => i32 { return 0; }" );
           ];
         types "implicit fields inside captured this lambda"
           [
             ( [ "helper" ],
               "export class Box { let value: i32 = 42; fun read() => i32 { \
                let f: () -> i32 = fn[this]() { return value; }; return f(); } \
                }" );
             main "let b = new helper.Box {}; return b.read();";
           ];
         ( "distinct nominal C signature conflict" >:: fun _ ->
           let program =
             resolved
               [
                 ( [ "left" ],
                   "class Box { let value: i32 = 0; } @C fun consume(box: Box) \
                    => i32;" );
                 ( [ "right" ],
                   "class Box { let value: i32 = 0; } @C fun consume(box: Box) \
                    => i32;" );
                 ( [ "main" ],
                   "import left; import right; fun main() => i32 { return 0; }"
                 );
               ]
           in
           match Typing.Type.type_prog program with
           | Ok _ ->
               assert_failure
                 "Nominally different C parameter types must conflict"
           | Error error ->
               assert_contains ~substring:"Conflicting @C signatures"
                 (Core.Error.to_string_hum error) );
         ( "non-default constructor signature" >:: fun _ ->
           let program =
             resolved
               [
                 ( [ "helper" ],
                   "export class Box { let value: i32 = 0; fun Box(value: i32) \
                    => Box { return new Box {}; } }" );
                 main "let b: helper.Box; return b.value;";
               ]
           in
           match Typing.Type.type_prog program with
           | Ok _ ->
               assert_failure
                 "A default initializer cannot supply constructor arguments"
           | Error error ->
               assert_contains ~substring:"must take no arguments"
                 (Core.Error.to_string_hum error) );
         ( "C signatures conflict" >:: fun _ ->
           let program =
             resolved
               [
                 ([ "left" ], "@C fun puts(text: string) => i32;");
                 ([ "right" ], "@C fun puts(text: i32) => i32;");
                 ( [ "main" ],
                   "import left; import right; fun main() => i32 { return 0; }"
                 );
               ]
           in
           match Typing.Type.type_prog program with
           | Ok _ -> assert_failure "Expected C signature conflict"
           | Error error ->
               let message = Core.Error.to_string_hum error in
               assert_contains ~substring:"Conflicting @C signatures for puts"
                 message;
               assert_contains ~substring:"right.bl" message;
               assert_contains ~substring:"left.bl" message );
         ( "nominal mismatch uses readable names" >:: fun _ ->
           let program =
             resolved
               [
                 ([ "left" ], "export class Box { let value: i32; }");
                 ([ "right" ], "export class Box { let value: i32; }");
                 ( [ "main" ],
                   "import left; import right; fun main() => i32 { let b: \
                    left.Box = new right.Box { value = 42 }; return 0; }" );
               ]
           in
           match Typing.Type.type_prog program with
           | Ok _ -> assert_failure "Expected distinct nominal types"
           | Error error ->
               let message =
                 Modules.Module_symbols.display_names
                   (Core.Error.to_string_hum error)
               in
               assert_contains ~substring:"left.Box" message;
               assert_contains ~substring:"right.Box" message );
         rejects "private function" "private"
           [
             ([ "helper" ], "fun answer() => i32 { return 42; }");
             main "return helper.answer();";
           ];
         rejects "private class" "private"
           [
             ([ "helper" ], "class Box { let value: i32; }");
             main "let b: helper.Box = new helper.Box {}; return 0;";
           ];
         rejects "missing member" "has no member missing"
           [ helper; main "return helper.missing();" ];
         rejects "module is not a value" "not a value"
           [ helper; main "let value = helper; return 0;" ];
         rejects "function is not a type" "not the required class or interface type"
           [ helper; main "let value: helper.answer; return 0;" ];
         rejects "class is not a function" "not a function"
           [
             ([ "helper" ], "export class Box { let value: i32; }");
             main "helper.Box(); return 0;";
           ];
         rejects "no unqualified imports" "Unknown name answer"
           [ helper; main "return answer();" ];
         rejects "no transitive names" "Unknown name answer"
           [
             helper;
             ( [ "middle" ],
               "import helper; export fun wrapped() => i32 { return \
                helper.answer(); }" );
             ( [ "main" ],
               "import middle; fun main() => i32 { return answer(); }" );
           ];
         rejects "no reexport of aliases" "has no member helper"
           [
             helper;
             ( [ "middle" ],
               "import helper; export fun wrapped() => i32 { return \
                helper.answer(); }" );
             ( [ "main" ],
               "import middle; fun main() => i32 { return \
                middle.helper.answer(); }" );
           ];
         rejects "duplicate alias" "Duplicate import alias"
           [
             helper;
             ( [ "main" ],
               "import helper; import helper; fun main() => i32 { return 0; }"
             );
           ];
         rejects "declaration alias collision" "conflicts with a declaration"
           [
             helper;
             ( [ "main" ],
               "import helper; fun helper() => i32 { return 0; } fun main() => \
                i32 { return 0; }" );
           ];
         rejects "parameter alias collision" "conflicts with an import alias"
           [
             helper;
             ( [ "main" ],
               "import helper; fun test(helper: i32) => i32 { return helper; } \
                fun main() => i32 { return 0; }" );
           ];
         rejects "local alias collision" "conflicts with an import alias"
           [ helper; main "let helper = 0; return helper;" ];
         rejects "loop alias collision" "conflicts with an import alias"
           [ helper; main "for helper in [1,2] {} return 0;" ];
         rejects "lambda alias collision" "conflicts with an import alias"
           [
             helper;
             main
               "let f: (i32) -> i32 = fn[](helper) { return helper; }; return \
                0;";
           ];
         rejects "uncaptured local" "Unknown name value"
           [
             helper;
             main
               "let value = 42; let f: () -> i32 = fn[]() { return value; }; \
                return f();";
           ];
         rejects "imported main" "Only the entry module"
           [
             ([ "helper" ], "fun main() => i32 { return 0; }"); main "return 0;";
           ];
         rejects "C definition collision" "Duplicate declaration"
           [
             ( [ "main" ],
               "@C fun puts(text: string) => i32; fun puts(text: string) => \
                i32 { return 0; } fun main() => i32 { return 0; }" );
           ];
         rejects "third prototype or definition" "Duplicate declaration"
           [
             ( [ "main" ],
               "fun value() => i32; fun value() => i32 { return 0; } fun \
                value() => i32; fun main() => i32 { return 0; }" );
           ];
       ]

let () = run_test_tt_main suite
