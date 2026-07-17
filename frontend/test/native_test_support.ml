open OUnit2

let executable_path relative_path =
  let test_dir = Filename.dirname Sys.executable_name in
  let path = Filename.concat test_dir relative_path in
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let assert_success label command =
  match Core_unix.system command with
  | Ok () -> ()
  | status ->
      assert_failure
        (Printf.sprintf "%s failed: %s\ncommand: %s" label
           (Core_unix.Exit_or_signal.to_string_hum status)
           command)

let assert_success_silently label command =
  let stdout_file = "command.stdout" in
  let stderr_file = "command.stderr" in
  let redirected =
    Printf.sprintf "%s > %s 2> %s" command
      (Filename.quote stdout_file)
      (Filename.quote stderr_file)
  in
  match Core_unix.system redirected with
  | Ok () -> ()
  | status ->
      let stdout = Core.In_channel.read_all stdout_file in
      let stderr = Core.In_channel.read_all stderr_file in
      assert_failure
        (Printf.sprintf "%s failed: %s\nstdout:\n%s\nstderr:\n%s\ncommand: %s"
           label
           (Core_unix.Exit_or_signal.to_string_hum status)
           stdout stderr command)

let assert_exit_code expected command =
  match (expected, Core_unix.system command) with
  | 0, Ok () -> ()
  | expected, Error (`Exit_non_zero actual) ->
      assert_equal ~printer:string_of_int ~msg:"native executable exit status"
        expected actual
  | _, status ->
      assert_failure
        (Printf.sprintf "executable terminated unexpectedly: %s"
           (Core_unix.Exit_or_signal.to_string_hum status))

let assert_nonempty_file filename =
  match Core.In_channel.read_all filename with
  | "" -> assert_failure (filename ^ " was empty")
  | _ -> ()
  | exception Sys_error message -> assert_failure message

let compile_and_run ~expected_exit =
  assert_nonempty_file "new_output.ll";
  assert_success "LLVM object generation"
    "llc --filetype=obj --relocation-model=pic new_output.ll -o program.o";
  assert_nonempty_file "program.o";
  assert_success "native linking" "clang program.o -o program -lm";
  assert_nonempty_file "program";
  assert_exit_code expected_exit "./program"

let in_temp_dir ~prefix test_context callback =
  let temp_dir = bracket_tmpdir ~prefix test_context in
  with_bracket_chdir test_context temp_dir (fun _ -> callback ())
