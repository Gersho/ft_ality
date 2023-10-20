let main () =
  let debug : bool =
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

  if debug then print_endline "debug enabled.";

  print_string "Opening file: ";
  print_endline Sys.argv.(1);
  let result =
    try
      let gmr_file = open_in Sys.argv.(1) in
      let result =
        Parser.parse gmr_file
      in
      close_in gmr_file;
      result
    with
    | e -> raise e
  in

  if debug then Debug.print_machine result.machine;
  Debug.print_key_mappings result.keyconfig;

  (try
     Sdl.init [`VIDEO];
     ignore (Sdl.Render.create_window_and_renderer ~width:0 ~height:0 ~flags:[Sdlwindow.Borderless]);
     at_exit Sdl.quit
   with
  | e -> raise e);

  print_endline "#### Machine Start ####";
  Machine.run result debug


let () = main ()
