open OUnit2
open Module_test_support
open Ast

let helper = ([ "helper" ], "export fun answer() => i32 { return 42; }")
let main body = ([ "main" ], "import helper; fun main() => i32 { " ^ body ^ " }")

let named_main body =
  ([ "main" ], "import answer from helper; fun main() => i32 { " ^ body ^ " }")

let rejects name substring sources =
  name >:: fun _ -> assert_resolution_error ~substring sources

let types name sources = name >:: fun _ -> assert_types sources

let suite =
  "module resolver"
  >::: [
         types "named and qualified aliases preserve defining module scope"
           [
             ( [ "helper" ],
               {|class Hidden { let value: i32; }
export type Number = i32;
export type Callback = (Number) -> Number;
export type PublicBox = Hidden;
export interface Value<T> { let value: T; }
export type IntValue = Value<Number>;|}
             );
             ( [ "main" ],
               {|import Number, Callback, PublicBox, IntValue from helper;
import helper as h;
class Hidden { let other: bool; }
class Local impl IntValue { let value: Number; }
fun read<T impl h.IntValue>(value: T) => Number { return value.value; }
fun main() => i32 {
  let box: h.PublicBox = new PublicBox { value = 40 };
  let f: Callback = fn[box](x: h.Number) -> Number { return box.value + x; };
  let local = new Local { value = 2 };
  let result = f(read(local));
  free f, box, local;
  return result;
}|}
             );
           ];
         rejects "private alias" "is private"
           [
             ([ "helper" ], "type Number = i32;");
             ([ "main" ], "import helper; fun f(value: helper.Number) {}");
           ];
         rejects "unused private named alias" "is private"
           [
             ([ "helper" ], "type Number = i32;");
             ([ "main" ], "import Number from helper;");
           ];
         rejects "alias import collision" "conflicts with a declaration"
           [
             helper;
             ([ "main" ], "import answer from helper; type answer = i32;");
           ];
         rejects "alias declaration collision" "Duplicate declaration"
           [ ([ "main" ], "type Box = i32; class Box {}") ];
         rejects "alias is not a value" "not a function or value"
           [
             ( [ "main" ],
               "type Number = i32; fun main() => i32 { return Number; }" );
           ];
         rejects "qualified alias is not a value" "not a function or value"
           [
             ([ "helper" ], "export type Number = i32;");
             ( [ "main" ],
               "import helper; fun main() => i32 { return helper.Number; }" );
           ];
         rejects "aliases do not reexport named imports" "has no member Number"
           [
             ([ "helper" ], "export type Number = i32;");
             ([ "middle" ], "import Number from helper;");
             ([ "main" ], "import middle; fun f(value: middle.Number) {}");
           ];
         types "named imports coexist with qualified imports"
           [
             helper;
             ( [ "main" ],
               "import answer from helper; import helper as h; fun main() => \
                i32 { let f: () -> i32 = answer; return f() + h.answer(); }" );
           ];
         types "named nominal types and dependent generic bounds"
           [
             ( [ "contracts" ],
               {|export interface Value<T> { let item: T; }
export class Box<T> impl Value<T> { let item: T; }
export fun read<T impl Value<U>, U>(value: T, fallback: U) => U { return value.item; }|}
             );
             ( [ "main" ],
               {|import Value, Box, read, from contracts;
class Local impl Value<i32> { let item: i32 = 42; }
fun main() => i32 {
  let box: Box<i32> = new Box<i32> { item = 42 };
  let view: Value<i32> = box;
  return read(view, 0);
}|}
             );
           ];
         types "named globals in constant and field initializers"
           [
             ([ "state" ], "export const seed = 42; export let count = 0;");
             ( [ "main" ],
               "import seed, count from state; const answer = seed; class Box \
                { let value = seed; } fun main() => i32 { count = answer; \
                return count; }" );
           ];
         types "locals parameters fields and lambdas shadow named imports"
           [
             helper;
             ( [ "main" ],
               {|import answer from helper;
class Box { let answer: i32 = 42; fun get() => i32 { return answer; } }
fun read(answer: i32) => i32 { return answer; }
fun main() => i32 {
  let result = answer();
  let answer = result;
  let f: (i32) -> i32 = fn[answer](value) { return answer + value; };
  let g: (i32) -> i32 = fn[](answer) { return answer; };
  for answer in [0] { let copy = answer; }
  return read(f(g(0)));
}|}
             );
           ];
         rejects "named import does not bind the module" "Unknown name helper"
           [ helper; named_main "return helper.answer();" ];
         rejects "named import exposes only selected members"
           "Unknown name other"
           [
             ( [ "helper" ],
               "export fun answer() => i32 { return 42; } export fun other() \
                => i32 { return 0; }" );
             named_main "return other();";
           ];
         rejects "unused private named import" "is private"
           [
             ([ "helper" ], "fun answer() => i32 { return 42; }");
             named_main "return 0;";
           ];
         rejects "unused missing named import" "has no member answer"
           [ ([ "helper" ], ""); named_main "return 0;" ];
         rejects "duplicate named import in list" "Duplicate named import"
           [ helper; ([ "main" ], "import answer, answer from helper;") ];
         rejects "duplicate named import across declarations"
           "Duplicate named import"
           [
             helper;
             ( [ "main" ],
               "import answer from helper; import answer from helper;" );
           ];
         rejects "same named import from different modules"
           "Duplicate named import"
           [
             helper;
             ([ "other" ], "export fun answer() {}");
             ([ "main" ], "import answer from helper; import answer from other;");
           ];
         rejects "named import conflicts with a declaration"
           "conflicts with a declaration"
           [
             helper; ([ "main" ], "import answer from helper; let answer = 0;");
           ];
         rejects "named import conflicts with an alias"
           "conflicts with an existing import alias"
           [
             helper;
             ([ "main" ], "import helper as answer; import answer from helper;");
           ];
         rejects "alias conflicts with a named import"
           "conflicts with an existing named import"
           [
             helper;
             ([ "main" ], "import answer from helper; import helper as answer;");
           ];
         rejects "named imports are not reexported" "has no member answer"
           [
             helper;
             ([ "middle" ], "import answer from helper;");
             ([ "main" ], "import answer from middle;");
           ];
         rejects "named import class is not a value" "not a function or value"
           [
             ([ "helper" ], "export class Box {}");
             ([ "main" ], "import Box from helper; fun main() { Box(); }");
           ];
         rejects "named import function is not a type"
           "not the required class or interface type"
           [ helper; named_main "let value: answer; return 0;" ];
         ( "named import diagnostics point to the imported name" >:: fun _ ->
           List.iter
             (fun (declarations, imports, substring, expected_loc) ->
               match
                 resolve [ ([ "helper" ], declarations); ([ "main" ], imports) ]
               with
               | Ok _ -> assert_failure "expected invalid named import"
               | Error diagnostic ->
                   assert_contains ~substring diagnostic.message;
                   assert_equal expected_loc diagnostic.loc)
             [
               ( "",
                 "\nimport missing from helper;",
                 "has no member missing",
                 ("main.bl", (2, 8), (2, 15)) );
               ( "let hidden = 0;",
                 "\nimport hidden from helper;",
                 "is private",
                 ("main.bl", (2, 8), (2, 14)) );
               ( "export const x = 0;",
                 "\nimport x, x from helper;",
                 "Duplicate named import",
                 ("main.bl", (2, 11), (2, 12)) );
             ] );
         types
           "constraints resolve at the definition site with dependent arguments"
           [
             ( [ "contracts" ],
               "export interface Value<T> {} export class Holder<T impl \
                Value<U>, U> { let item: T; } export fun keep<T impl Value<U>, \
                U>(x: T, y: U) => T { return x; }" );
             ( [ "main" ],
               "import contracts as c; interface Value<T> {} class Box impl \
                c.Value<i32> {} fun main() => i32 { let box = new Box {}; let \
                holder = new c.Holder<Box, i32> { item = c.keep(box, 0) }; \
                free holder, box; return 42; }" );
           ];
         rejects "private interface in a constraint" "is private"
           [
             ([ "contracts" ], "interface Hidden {} ");
             ( [ "main" ],
               "import contracts; fun f<T impl contracts.Hidden>(x: T) => T { \
                return x; }" );
           ];
         types
           "generic interfaces preserve definition-site and parameter bindings"
           [
             ( [ "contracts" ],
               "class Hidden {} export interface Value<Element> { const item: \
                Element; fun get() => Element; fun hidden() => Hidden; }" );
             ( [ "main" ],
               "import contracts as c; class Element {} fun read<T>(x: \
                c.Value<T>) => T { return x.get(); }" );
           ];
         types "qualified generic implementation supports inference"
           [
             ( [ "contracts" ],
               "export interface Value<T> { fun get() => T; } export fun \
                read<T>(x: Value<T>) => T { return x.get(); }" );
             ( [ "main" ],
               "import contracts as c; class Box<T> impl c.Value<T> { let \
                item: T; fun get() => T { return item; } } fun main() => i32 { \
                return c.read(new Box<i32> { item = 42 }); }" );
           ];
         types
           "same generic interface spelling in different modules stays distinct"
           [
             ([ "left" ], "export interface I<T> { fun get() => T; }");
             ([ "right" ], "export interface I<T> { fun get() => T; }");
             ( [ "main" ],
               "import left; import right; class C impl left.I<i32>, \
                right.I<i32> { fun get() => i32 { return 42; } } fun main() => \
                i32 { let x: left.I<i32> = new C {}; let y: right.I<i32> = new \
                C {}; return x.get(); }" );
           ];
         rejects "private generic interface in annotation" "is private"
           [
             ([ "contracts" ], "interface I<T> {}");
             ([ "main" ], "import contracts; fun f(x: contracts.I<i32>) {}");
           ];
         rejects "private generic interface in impl" "is private"
           [
             ([ "contracts" ], "interface I<T> {}");
             ([ "main" ], "import contracts; class C impl contracts.I<i32> {}");
           ];
         rejects "generic interface cannot be constructed"
           "not the required class"
           [
             ([ "contracts" ], "export interface I<T> {}");
             ( [ "main" ],
               "import contracts; fun f() { let x = new contracts.I<i32> {}; }"
             );
           ];
         rejects "generic class cannot be implemented" "not the required class"
           [
             ([ "contracts" ], "export class C<T> {}");
             ([ "main" ], "import contracts; class D impl contracts.C<i32> {}");
           ];
         types "same generic class spelling remains distinct across modules"
           [
             ([ "left" ], "export class Box<T> { let item: T; }");
             ([ "right" ], "export class Box<T> { let item: T; let extra = 1; }");
             ( [ "main" ],
               "import left; import right; fun main() => i32 { let x = new \
                left.Box<i32> { item = 20 }; let y = new right.Box<i32> { item \
                = 22 }; return x.item + y.item; }" );
           ];
         types
           "generic classes resolve at definition site and preserve parameter \
            scope"
           [
             ( [ "helper" ],
               "fun secret() => i32 { return 42; } export class Box<Element> { \
                let item: Element; fun value() => i32 { return secret(); } } \
                export fun wrap<T>(x: T) => Box<T> { return new Box<T> { item \
                = x }; }" );
             ( [ "main" ],
               "import helper as h; class Element {} fun secret() => i32 { \
                return 1; } fun main() => i32 { let x: h.Box<i32> = h.wrap(0); \
                let y = new h.Box<bool> { item = true }; return x.value(); }" );
           ];
         rejects "private generic class" "is private"
           [
             ([ "helper" ], "class Box<T> { let item: T; }");
             ([ "main" ], "import helper; fun f(x: helper.Box<i32>) {}");
           ];
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
         types "generic definition-site bindings and parameter scope"
           [
             ( [ "helper" ],
               "fun secret() => i32 { return 42; } export fun answer<T>(value: \
                T) => i32 { let copy: T = value; return secret(); }" );
             ( [ "main" ],
               "import helper; class T {} fun secret() => i32 { return 1; } \
                fun main() => i32 { return helper.answer(0); }" );
           ];
         types "generic type parameter shadows class"
           [
             ( [ "main" ],
               "class T {} fun identity<T>(value: T) => T { return value; } \
                fun main() => i32 { return identity(42); }" );
           ];
         types "generic imported function value"
           [
             ( [ "helper" ],
               "export fun identity<T>(value: T) => T { return value; }" );
             ( [ "main" ],
               "import helper; fun main() => i32 { let f: (i32) -> i32 = \
                helper.identity; return f(42); }" );
           ];
         rejects "generic private export" "private"
           [
             ([ "helper" ], "fun identity<T>(value: T) => T { return value; }");
             ( [ "main" ],
               "import helper; fun main() => i32 { return helper.identity(42); \
                }" );
           ];
         rejects "generic parameter does not leak" "Unknown name"
           [
             ( [ "main" ],
               "fun identity<T>(value: T) => T { return value; } fun \
                bad(value: T) {}" );
           ];
         types "global reads writes and imported constant initializer"
           [
             ([ "state" ], "export let count = 40; export const limit: i32 = 2;");
             ( [ "main" ],
               "import state as shared; const increment = shared.limit; fun \
                main() => i32 { shared.count += increment; return \
                shared.count; }" );
           ];
         types "global same spellings and local shadowing"
           [
             ([ "left" ], "export let value = 20;");
             ([ "right" ], "export let value = 22;");
             ( [ "main" ],
               "import left; import right; let value = 9; fun read(value: i32) \
                => i32 { return value; } fun main() => i32 { let value = 3; \
                return left.value + right.value + read(value); }" );
           ];
         types "global lambda direct access and capture"
           [
             ([ "state" ], "export let count = 40;");
             ( [ "main" ],
               "import state; let count = 40; fun main() => i32 { let direct: \
                () -> i32 = fn[]() { return state.count + count; }; let \
                captured: () -> i32 = fn[count]() { return count; }; return \
                direct() + captured(); }" );
           ];
         rejects "global private access" "private"
           [
             ([ "state" ], "let secret = 42;");
             ( [ "main" ],
               "import state; fun main() => i32 { return state.secret; }" );
           ];
         rejects "global initializer private access" "private"
           [
             ([ "state" ], "const secret = 42;");
             ( [ "main" ],
               "import state; let value = state.secret; fun main() => i32 { \
                return value; }" );
           ];
         rejects "global alias collision" "conflicts with a declaration"
           [
             helper;
             ( [ "main" ],
               "import helper; let helper = 42; fun main() => i32 { return 0; }"
             );
           ];
         rejects "duplicate global" "Duplicate declaration"
           [
             ( [ "main" ],
               "let value = 20; const value = 22; fun main() => i32 { return \
                0; }" );
           ];
         rejects "global function duplicate" "Duplicate declaration"
           [
             ( [ "main" ],
               "let value = 20; fun value() => i32 { return 22; } fun main() \
                => i32 { return 0; }" );
           ];
         rejects "global main is reserved" "main"
           [ ([ "main" ], "let main = 42;") ];
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
           let functions, _, _, _ =
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
         rejects "function is not a type"
           "not the required class or interface type"
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
