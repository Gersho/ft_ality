let rec get_input () : string =
  match Sdl.Event.poll_event () with
  | Some (Sdl.Event.KeyDown evt) -> Sdlkeycode.to_string evt.keycode
  | Some (Sdl.Event.Quit _) -> exit 0
  | _ -> get_input ()


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
  try
    let gmr_file = open_in Sys.argv.(1) in
    let result =
      Parser.parse gmr_file Head 1 { keyconfig = []; machine = [{ id = 0; transitions = [] }] }
    in
    let final_parsed : Types.full_config =
      let machine_sorted =
        List.sort (fun (a : Types.state) (b : Types.state) -> compare a.id b.id) result.machine
      in
      { keyconfig = result.keyconfig; machine = machine_sorted }
    in

    if List.length final_parsed.machine == 1 then (
      print_endline "no valid configuration found";
      exit 3);

    if debug then Debug.print_machine final_parsed.machine;
    Debug.print_key_mappings final_parsed.keyconfig;

    Sdl.init [`VIDEO];
    ignore (Sdl.Render.create_window_and_renderer ~width:0 ~height:0 ~flags:[Sdlwindow.Borderless]);
    at_exit Sdl.quit;

    print_endline "#### Machine Start ####";
    let rec machine_test state =
      let key = get_input () in
      let rec process_input state = function
        | Some (input : Types.key) -> (
            let transitions = (List.nth final_parsed.machine state).transitions in
            match
              List.find_opt
                (fun (el : Types.transition) -> String.equal input.output_string el.read)
                transitions
            with
            | Some transi -> (
                if debug then Debug.print_has_transition state transi;
                match transi.write with
                | "" -> machine_test transi.to_state
                | str ->
                    print_endline transi.write;
                    machine_test transi.to_state)
            | None ->
                if debug then Debug.print_no_transition input.output_string state;
                if state == 0 then machine_test 0 else process_input 0 (Some input))
        | None -> machine_test state
      in
      List.find_opt
        (fun (el : Types.key) -> String.equal el.input_string key)
        final_parsed.keyconfig
      |> process_input state
    in
    machine_test 0
  with
  | e -> raise e


let () = main ()
