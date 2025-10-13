let test_file_path = 
  (* surely there is a better way to read the test files... *)
  let cwd = Sys.getcwd () in 
  Filename.concat cwd "../../../../examples"


let test_file_channel f = 
  Filename.concat test_file_path f |> Core.In_channel.read_all