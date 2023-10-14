let print_machine (config : Types.full_config) =
  (* debug print *)
  print_endline "debug print: ";
  print_endline "keyconf: ";
  let print_keyconf (elem : Types.key) =
    print_string "key.input: ";
    print_string elem.input_string;
    print_string " key.output: ";
    print_endline elem.output_string
  in
  List.iter print_keyconf config.keyconfig;
  print_endline "machine: ";
  let print_transition (elem : Types.transition) =
    print_string "     read: ";
    print_string elem.read;
    print_string " to_state: ";
    print_int elem.to_state;
    print_string " write: ";
    print_endline elem.write
  in
  let print_state (elem : Types.state) =
    print_string "id: ";
    print_int elem.id;
    print_endline "";
    List.iter print_transition elem.transitions
  in
  List.iter print_state config.machine


let rec get_input () : string =
  match Sdl.Event.poll_event () with
  | Some (Sdl.Event.KeyDown evt) -> Sdlkeycode.to_string evt.keycode
  | Some (Sdl.Event.Quit _) -> exit 0
  | _ -> get_input ()


let main () =
  (* Array forbiden module ?! *)
  (* TODO complete arg handling (help, debug, syntax) *)
  if Array.length Sys.argv < 2 then (
    print_endline "Wrong amount of arguments";
    exit 1);
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

    print_machine final_parsed;

    Sdl.init [`VIDEO];
    ignore (Sdl.Render.create_window_and_renderer ~width:0 ~height:0 ~flags:[Sdlwindow.Borderless]);
    at_exit Sdl.quit;

    print_endline "#### START MACHINE ####";
    let rec machine_test state =
      let key = get_input () in
      let rec process_input state = function
        | Some (input : Types.key) -> (
            let a = (List.nth final_parsed.machine state).transitions in
            match
              List.find_opt
                (fun (el : Types.transition) -> String.equal input.output_string el.read)
                a
            with
            | Some transi -> (
                print_string "input: ";
                print_string input.output_string;
                print_string " has a follow-up in state: ";
                print_int state;
                print_string " pointing to state ";
                print_int transi.to_state;
                print_endline "";

                match transi.write with
                | "" -> machine_test transi.to_state
                | str ->
                    print_endline transi.write;
                    machine_test transi.to_state)
            | None ->
                print_string "input: ";
                print_string input.output_string;
                print_string " has no follow-up in state: ";
                print_int state;
                print_endline " -> going back to state 0";
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
