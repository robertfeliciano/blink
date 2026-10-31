open OUnit2
open Ast
module T = Typing.Typed_ast

let parse source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "aliases.bl" };
  match Parsing.Parse.parse_prog lexbuf with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let type_program source =
  match Typing.Type.type_prog (parse source) with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let rejects name substring source =
  name >:: fun _ ->
  match Typing.Type.type_prog (parse source) with
  | Ok _ -> assert_failure "expected alias type checking to fail"
  | Error error ->
      let message = Core.Error.to_string_hum error in
      List.iter
        (fun substring ->
          assert_bool
            ("expected " ^ substring ^ " in " ^ message)
            (Core.String.is_substring message ~substring))
        [ "Type Error:"; "aliases.bl:"; substring ];
      assert_bool "failure must be a source diagnostic"
        (not (Core.String.is_substring message ~substring:"Internal"))

let test_parser _ =
  let program = parse "export type Callback = (i32) -> i32;" in
  match program with
  | Prog
      ( [],
        [ { elt = { declaration = TypeAlias alias; export_loc = Some _ }; _ } ]
      ) ->
      assert_equal "Callback" alias.elt.aname.elt;
      assert_equal ("aliases.bl", (1, 13), (1, 21)) alias.elt.aname.loc;
      assert_equal ("aliases.bl", (1, 24), (1, 36)) alias.elt.target.loc;
      assert_equal
        (TRef (RFun ([ TInt (TSigned Ti32) ], RetVal (TInt (TSigned Ti32)))))
        alias.elt.target.elt;
      assert_equal ([], [], [], []) (partition_declarations program);
      assert_bool "printer preserves the exported declaration"
        (Core.String.is_prefix (show_prog program)
           ~prefix:"Program{\nexport type Callback = ")
  | _ -> assert_failure "alias declaration or export metadata was lost"

let test_parser_boundaries _ =
  List.iter
    (fun source ->
      match Parsing.Parse.parse_prog (Lexing.from_string source) with
      | Error _ -> ()
      | Ok _ -> assert_failure ("expected parse rejection: " ^ source))
    [
      "type Bad = i32";
      "type Bad = void;";
      "type Bad<T> = T;";
      "type Bad = distinct i64;";
      "fun main() { type Local = i32; }";
      "let type = 42;";
    ];
  ignore
    (parse "type Nested = Box<Box<i32>>; type Returning = () -> (i32) -> i32;")

let test_transparency _ =
  let body =
    {|const seed: Number = 40;
fun apply(callback: Callback, value: Number) => Number { return callback(value); }
fun main() => i32 {
  let values: Pair = [seed, 2];
  return values[0] + values[1];
}|}
  in
  let aliases =
    type_program
      ("type Pair = [Number; 2]; type Callback = (Number) -> Number; type \
        Number = i32;" ^ body)
  in
  let expanded =
    type_program
      "const seed: i32 = 40; fun apply(callback: (i32) -> i32, value: i32) => \
       i32 { return callback(value); } fun main() => i32 { let values: [i32; \
       2] = [seed, 2]; return values[0] + values[1]; }"
  in
  assert_equal ~printer:Typing.Pprint_typed_ast.show_typed_program expanded
    aliases;
  let lower program =
    let counter = Desugaring.Desugar_util.counter in
    let previous = !counter in
    Fun.protect
      ~finally:(fun () -> counter := previous)
      (fun () ->
        match Desugaring.Desugar.desugar_prog program with
        | Ok program -> program
        | Error error -> assert_failure (Core.Error.to_string_hum error))
  in
  assert_equal ~printer:Desugaring.Pprint_desugared_ast.show_desugared_program
    (lower expanded) (lower aliases)

let test_generics_and_interfaces _ =
  match
    type_program
      {|type Small = i32;
type IntBox = Box<Small>;
type View = Value<Small>;
interface Value<T> { let value: T; }
class Box<T> impl Value<T> { let value: T; }
class Other impl View { let value: Small; }
fun identity<T>(value: T) => T { return value; }
fun read<T impl View>(value: T) => Small { return value.value; }
fun main() => i32 {
  let box: Box<i32> = new IntBox { value = 40 };
  let same: IntBox = identity(box);
  let view: View = same;
  let other = new Other { value = 2 };
  let result = read(view) + read(other);
  free box, other;
  return result;
}|}
  with
  | T.Prog (_, _, classes, _, _, interfaces) ->
      assert_equal 2 (List.length classes);
      assert_equal 1 (List.length interfaces)

let test_annotations_and_shadowing _ =
  ignore
    (type_program
       {|type T = i32;
type Callback = (T) -> T;
type Thing = Box;
class Box { let value: T; }
fun identity<T>(value: T) => T { return value; }
fun main() => i32 {
  let T = 40;
  let box: Thing = new Thing { value = T };
  let f: Callback = fn[box](x: T) -> T { let y: T = identity(x); return box.value + y; };
  let result = f(2);
  free f, box;
  return result;
}|})

let suite =
  "Type aliases"
  >::: [
         "parser and ranges" >:: test_parser;
         "parser boundaries" >:: test_parser_boundaries;
         "typed and lowered transparency" >:: test_transparency;
         "nominal generics and interface bounds"
         >:: test_generics_and_interfaces;
         "nested annotations and parameter shadowing"
         >:: test_annotations_and_shadowing;
         rejects "direct cycle" "Cyclic type alias: A -> A" "type A = A;";
         rejects "nested cycle" "A -> B -> A"
           "type A = [B; 2]; type B = (A) -> i32;";
         rejects "cycle through generic argument" "Cyclic type alias"
           "class Box<T> { let value: T; } type A = Box<A>;";
         rejects "unknown unused alias" "class undefined"
           "type Unused = Missing;";
         rejects "unknown nested type" "class undefined"
           "type Unused = (i32) -> [Missing; 2];";
         rejects "free parameter" "class undefined"
           "type Unused = T; fun identity<T>(value: T) => T { return value; }";
         rejects "duplicate alias" "Duplicate type alias"
           "type A = i32; type A = i64;";
         rejects "class collision" "Duplicate declaration"
           "class A {} type A = i32;";
         rejects "function collision" "Duplicate declaration"
           "type A = i32; fun A() {}";
         rejects "global collision" "Duplicate declaration"
           "let A = 0; type A = i32;";
         rejects "alias application" "does not accept type arguments"
           "type A = i32; fun f(value: A<i32>) {}";
         rejects "missing generic arguments" "requires type arguments"
           "class Box<T> {} type A = Box;";
         rejects "extra generic arguments" "expects 1 type arguments"
           "class Box<T> {} type A = Box<i32, i64>;";
         rejects "invalid unused bound" "does not satisfy interface constraint"
           "interface Value {} class Box<T impl Value> {} type Bad = Box<i32>;";
         rejects "construct scalar" "Object construction requires a class type"
           "type A = i32; fun main() { let value = new A {}; }";
         rejects "construct array" "Object construction requires a class type"
           "type A = [i32; 1]; fun main() { let value = new A {}; }";
         rejects "construct interface" "Cannot construct interface"
           "interface Value {} type A = Value; fun main() { let value = new A \
            {}; }";
         rejects "implement scalar"
           "Expected a concrete interface implementation type"
           "type A = i32; class Box impl A {}";
         rejects "scalar bound" "constraint must be an interface"
           "type A = i32; fun f<T impl A>(value: T) {}";
         rejects "literal range" "256"
           "type Byte = u8; fun main() { let value: Byte = 256; }";
         rejects "function cast stays prohibited"
           "Cannot cast functions/lambdas"
           "type Callback = (i32) -> i32; fun f(x: i32) => i32 { return x; } \
            fun main() { let value = f as Callback; }";
         rejects "function field stays prohibited"
           "Lambdas not allowed at class field level"
           "type Callback = (i32) -> i32; class Box { let callback: Callback; }";
         rejects "aggregate global stays prohibited"
           "Globals currently support only"
           "type Values = [i32; 1]; let values: Values = [42];";
       ]
