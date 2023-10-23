let main () =
  let debug =
    match Sys.argv with
    | [| _; "--help" |] ->
        Help.print_help ();
        exit 1
    | [| _; "--file-syntax" |] ->
        Help.print_syntax ();
        exit 1
    | [| _; _ |] -> false
    | [| _; _; "--debug" |] -> true
    | [| _; _; _ |] ->
        print_string "Unknown option ";
        print_endline Sys.argv.(2);
        exit 1
    | _ ->
        print_endline "Wrong amount of arguments";
        exit 1
  in
  let result =
    try
      let gmr_file = open_in Sys.argv.(1) in
      let result = Parser.parse gmr_file in
      close_in gmr_file;
      result
    with
    | e -> raise e
  in

  if debug then Debug.print_machine result.machine;
  Debug.print_key_mappings result.keyconfig;

  print_endline "#### Machine Start ####";
  Input.init ();
  Machine.run result debug


let () = main ()
