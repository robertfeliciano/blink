open OUnit2
open Ast
module D = Desugaring.Desugared_ast

let parse source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "tuples.bl" };
  match Parsing.Parse.parse_prog lexbuf with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let type_program source =
  match Typing.Type.type_prog (parse source) with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let accepts name source = name >:: fun _ -> ignore (type_program source)

let rejects ?diagnostic name source =
  name >:: fun _ ->
  match Typing.Type.type_prog (parse source) with
  | Ok _ -> assert_failure "expected tuple type checking to fail"
  | Error error ->
      let message = Core.Error.to_string_hum error in
      List.iter
        (fun substring ->
          assert_bool
            ("expected " ^ substring ^ " in " ^ message)
            (Core.String.is_substring message ~substring))
        ([ "Type Error:"; "tuples.bl:" ] @ Option.to_list diagnostic);
      assert_bool "failure must be a source diagnostic"
        (not (Core.String.is_substring message ~substring:"Internal"))

let test_parser _ =
  let program =
    parse
      "fun main() { const (left, (_, right)): (i32, (bool, string)) = (42, \
       (true, \"ok\")); }"
  in
  let functions, _, _, _ = partition_declarations program in
  match functions with
  | [ { elt = { body = [ statement ]; _ }; _ } ] -> (
      match statement.elt with
      | Destructure
          ( { elt = PTuple [ { elt = PBind "left"; _ }; nested ]; loc },
            Some
              (TTuple [ TInt (TSigned Ti32); TTuple [ TBool; TRef RString ] ]),
            { elt = Tuple [ _; { elt = Tuple [ _; _ ]; _ } ]; _ },
            true ) -> (
          assert_equal ("tuples.bl", (1, 20), (1, 38)) loc;
          match nested.elt with
          | PTuple [ { elt = PBind "_"; _ }; { elt = PBind "right"; _ } ] -> ()
          | _ -> assert_failure "nested pattern was not preserved")
      | _ -> assert_failure "tuple syntax or annotation was not preserved")
  | _ -> assert_failure "expected one function containing one declaration"

let test_function_type_ambiguity _ =
  let program =
    parse
      "type Pair = (i32, i32); type Binary = (i32, i32) -> i32; type Unary = \
       ((i32, i32)) -> i32;"
  in
  match program with
  | Prog (_, declarations) ->
      let aliases =
        List.map
          (fun declaration ->
            match declaration.elt.declaration with
            | TypeAlias alias -> alias.elt.target.elt
            | _ -> assert_failure "expected alias")
          declarations
      in
      let i32 = TInt (TSigned Ti32) in
      assert_equal
        [
          TTuple [ i32; i32 ];
          TRef (RFun ([ i32; i32 ], RetVal i32));
          TRef (RFun ([ TTuple [ i32; i32 ] ], RetVal i32));
        ]
        aliases

let test_parser_boundaries _ =
  List.iter
    (fun source ->
      match Parsing.Parse.parse_prog (Lexing.from_string source) with
      | Error _ -> ()
      | Ok _ -> assert_failure ("expected parse rejection: " ^ source))
    [
      "fun main() { let tuple = (); }";
      "fun main() { let tuple = (1,); }";
      "fun main() { let (left: i32, right: i32) = (1, 2); }";
      "fun main() { let left = 1; let right = 2; (left, right) = (3, 4); }";
      "fun main() { for (left, right) in [(1, 2)] {} }";
    ]

let test_desugaring _ =
  let typed =
    type_program
      "fun make() => (i32, i32) { return (20, 22); } fun main() => i32 { const \
       (left, right) = make(); return left + right; }"
  in
  let lowered =
    match Desugaring.Desugar.desugar_prog typed with
    | Ok program -> program
    | Error error -> assert_failure (Core.Error.to_string_hum error)
  in
  let (D.Prog (_, functions, _, _, _, _)) = lowered in
  let main = List.find (fun (f : D.fdecl) -> f.fname = "main") functions in
  match main.body with
  | D.Decl (temporary, D.TTuple [ _; _ ], D.Call ("make", [], _), _)
    :: D.Decl ("left", _, D.TupleProj (D.Id (first, _), 0, _), true)
    :: D.Decl ("right", _, D.TupleProj (D.Id (second, _), 1, _), true)
    :: _ ->
      assert_equal temporary first;
      assert_equal temporary second
  | _ ->
      assert_failure
        ("destructuring must evaluate once into a temporary:\n"
        ^ Desugaring.Pprint_desugared_ast.show_desugared_program lowered)

let suite =
  "Tuples"
  >::: [
         "parser and pattern ranges" >:: test_parser;
         "tuple and function types" >:: test_function_type_ambiguity;
         "parser boundaries" >:: test_parser_boundaries;
         "single evaluation lowering" >:: test_desugaring;
         accepts "grouping and ordinary declarations"
           "fun main() => i32 { let value: i32 = (40); const add = 2; return \
            value + add; }";
         accepts "nested aliases generic inference and expected literals"
           "type Pair = (i32, bool); fun first<T,U>(pair: (T,U)) => T { return \
            pair[0]; } fun main() => i32 { let (value, (_, flag)): (u8, Pair) \
            = (40, (2, true)); return value + first((2, flag)); }";
         accepts "expected interface conversion"
           "interface Value { let value: i32; } class Box impl Value { let \
            value: i32; } fun read(value: (Value, i32)) => i32 { return \
            value[0].value + value[1]; } fun main() => i32 { let b = new Box \
            {value = 40}; return read((b, 2)); }";
         accepts "constant expression index"
           "fun main() => i32 { let pair = (20, 22); return pair[1 - 1] + \
            pair[1]; }";
         rejects "too many literal elements"
           "fun main() { let tuple = (1, 2, 3, 4); }";
         rejects "too many type elements" "type Bad = (i32, i32, i32, i32);";
         rejects "pattern arity mismatch"
           "fun main() { let (left, right) = (1, 2, 3); }";
         rejects "nested pattern arity mismatch"
           "fun main() { let (left, (a, b)) = (1, (2, 3, 4)); }";
         rejects "scalar initializer" "fun main() { let (left, right) = 42; }";
         rejects ~diagnostic:"Expected type TBool" "annotation mismatch"
           "fun main() { let (left, right): (i32, bool) = (1, 2); }";
         rejects ~diagnostic:"Variable left already exists"
           "duplicate nested binding"
           "fun main() { let (left, (left, right)) = (1, (2, 3)); }";
         rejects "too many pattern elements"
           "fun main() { let (a, b, c, d) = ((1, 2), 3); }";
         rejects "wildcard does not bind"
           "fun main() => i32 { let (_, _) = (1, 2); return _; }";
         rejects "initializer cannot see new bindings"
           "fun main() { let (left, right) = (1, left); }";
         rejects "dynamic index"
           "fun main() { let pair = (20, 22); let index = 1; let x = \
            pair[index]; }";
         rejects "negative index"
           "fun main() { let pair = (20, 22); let x = pair[-1]; }";
         rejects "out of bounds index"
           "fun main() { let pair = (20, 22); let x = pair[2]; }";
         rejects "folded negative index"
           "fun main() { let pair = (20, 22); let x = pair[~4294967295]; }";
         rejects "noninteger index"
           "fun main() { let pair = (20, 22); let x = pair[true]; }";
         rejects "immutable tuple slot"
           "fun main() { let pair = (20, 22); pair[0] = 42; }";
         rejects "immutable nested tuple slot"
           "fun main() { let pair = ((20, 22), 0); pair[0][1] += 1; }";
         rejects "constant destructured bindings"
           "fun main() { const (left, right) = (20, 22); left = 42; }";
         rejects "constant tuple binding"
           "fun main() { const pair = (20, 22); pair = (21, 21); }";
         rejects "structural tuple type mismatch"
           "fun main() { let pair: (i32, i32) = (20, 22); let other: (i64, \
            i64) = pair; }";
         rejects "tuple equality unsupported"
           "fun main() { let same = (1, 2) == (1, 2); }";
         rejects "tuple ordering unsupported"
           "fun main() { let same = (1, 2) < (1, 2); }";
         rejects "tuple global unsupported" "let pair = (20, 22);";
         rejects "C tuple parameter unsupported"
           "@C fun foreign(value: (i32, i32)) => i32;";
         rejects "C nested tuple parameter unsupported"
           "@C fun foreign(value: [(i32, i32); 1]) => i32;";
         rejects "C tuple return unsupported" "@C fun foreign() => (i32, i32);";
         rejects "C tuple definition parameter unsupported"
           "@C fun foreign(value: (i32, i32)) => i32 { return value[0]; }";
         rejects "C tuple definition return unsupported"
           "@C fun foreign() => (i32, i32) { return (20, 22); }";
       ]
