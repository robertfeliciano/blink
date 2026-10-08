let underapplication =
  List.concat_map
    (fun (return_name, return_type, body) ->
      let signature = "(left: i32, right: i32) => " ^ return_type in
      let lambda =
        "fn[](left: i32, right: i32) -> " ^ return_type ^ " { " ^ body ^ " }"
      in
      let function_type = "(i32, i32) -> " ^ return_type in
      let declaration = "fun target" ^ signature ^ " { " ^ body ^ " } " in
      let class_declaration =
        "class Box { fun target" ^ signature ^ " { " ^ body ^ " } "
      in
      let callees =
        [
          ("named", declaration, "", "target", "");
          ("function-value", declaration, "let f = target; ", "f", "");
          ("lambda", "", "let f = " ^ lambda ^ "; ", "f", "");
          ("direct-lambda", "", "", "(" ^ lambda ^ ")", "");
          ( "returned-function",
            "fun make() => " ^ function_type ^ " { return " ^ lambda ^ "; } ",
            "",
            "make()",
            "" );
          ( "method",
            class_declaration ^ "} ",
            "let box = new Box {}; ",
            "box.target",
            "" );
          ( "this-method",
            class_declaration ^ "fun check() => i32 { ",
            "",
            "this.target",
            " } } fun main() => i32 { return 0; }" );
          ("prototype", "@C fun target" ^ signature ^ "; ", "", "target", "");
          ( "function-argument",
            "fun check(f: " ^ function_type ^ ") => i32 { ",
            "",
            "f",
            " } fun main() => i32 { return 0; }" );
        ]
      in
      List.concat_map
        (fun (callee_name, declarations, setup, callee, suffix) ->
          List.concat_map
            (fun (argument_name, arguments) ->
              List.map
                (fun (context_name, context) ->
                  let name =
                    String.concat "-"
                      [ callee_name; return_name; argument_name; context_name ]
                  in
                  let main =
                    if suffix = "" then "fun main() => i32 { " else ""
                  in
                  let ending = if suffix = "" then " }" else suffix in
                  ( name,
                    declarations ^ main ^ setup ^ context ^ callee ^ "("
                    ^ arguments ^ "); return 0;" ^ ending ))
                [ ("expression", "let result = "); ("statement", "") ])
            [ ("zero", ""); ("one", "1") ])
        callees)
    [ ("value", "i32", "return left + right;"); ("void", "void", "") ]
