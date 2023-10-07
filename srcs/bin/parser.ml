type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

let rec parse (gmr_file : in_channel) (accum : Types.full_config) mode count =
  let parse_key (accum : Types.full_config) line count =
    let duplicate_check input (elem : Types.key) = String.equal elem.input_string input in
    match String.split_on_char ':' line with
    | input :: output
      when List.length output == 1
           && String.length input > 0
           && String.length (List.nth output 0) > 0
           && not (List.exists (duplicate_check input) accum.keyconfig) ->
      let new_rec : Types.key =
        { input_string = input; output_string = List.nth output 0 }
      in
      parse
        gmr_file
        { keyconfig = List.rev_append accum.keyconfig [ new_rec ]
        ; machine = accum.machine
        }
        Keyconfig
        (count + 1)
    | _ ->
      print_string "Syntax error on line ";
      print_int count;
      print_endline "";
      exit 3
  in
  match String.trim (input_line gmr_file) with
  (* skip empty lines *)
  | "" -> parse gmr_file accum mode (count + 1)
  (* switch parsing mode *)
  | line when String.equal line "@keyconfig" ->
    parse gmr_file accum Keyconfig (count + 1)
  | line when String.equal line "@movelist" ->
    parse gmr_file accum Movelist (count + 1)
  (* keyconfig parsing *)
  | line when mode == Keyconfig -> parse_key accum line count
  (* movelist parsing *)
  | line when mode == Movelist -> (
    match String.split_on_char ':' line with
    | actions_as_string :: move_name
      when List.length move_name == 1
           && String.length actions_as_string > 0
           && String.length (List.nth move_name 0) > 0 -> (
      let is_valid_key action (elem : Types.key) =
        String.equal elem.output_string action
      in
      match String.split_on_char '-' actions_as_string with
      | head :: tail
        when String.length head > 0
             && List.exists (is_valid_key head) accum.keyconfig ->
        let rec add_move action_head action_tail state_id (machine : Types.machine) =
          match action_head with
          | str
            when String.length str > 0
                 && List.exists (is_valid_key str) accum.keyconfig -> (
            let rec get_new_id acc (list : Types.machine) =
              match list with
              | [] -> acc + 1
              | hd :: tl ->
                if hd.id > acc then get_new_id hd.id tl
                else get_new_id acc tl
            in
            match List.find_opt (fun (el : Types.state) -> state_id == el.id) machine with
            | None ->
              print_string "Something went very wrong trying to parse line ";
              print_int count;
              print_endline " (should never display)";
              exit 4
            | Some state -> (
              match
                List.find_opt
                  (fun (el : Types.transition) -> String.equal action_head el.read)
                  state.transitions
              with
              | None -> (
                (* state has no transition for action_head *)
                let new_id = get_new_id 0 machine in
                let msg =
                  if List.length action_tail == 0 then List.nth move_name 0
                  else ""
                in
                let new_transition : Types.transition =
                  { read = action_head; to_state = new_id; write = msg }
                in
                let updated_current_state : Types.state =
                  { id = state_id
                  ; transitions =
                      List.rev_append state.transitions [ new_transition ]
                  }
                in
                let machine_without_current_state =
                  List.filter (fun (el : Types.state) -> not (state_id == el.id)) machine
                in
                let machine_with_updated_state =
                  List.rev_append
                    machine_without_current_state
                    [ updated_current_state ]
                in
                let new_state : Types.state = { id = new_id; transitions = [] } in
                match action_tail with
                | [] ->
                  List.rev_append machine_with_updated_state [ new_state ]
                | h :: t ->
                  add_move
                    h
                    t
                    new_id
                    (List.rev_append
                       machine_with_updated_state
                       [ new_state ]))
              | Some transi when String.equal transi.write "" -> (
                match action_tail with
                | [] ->
                  let msg = List.nth move_name 0 in
                  let new_transition : Types.transition =
                    { read = transi.read
                    ; to_state = transi.to_state
                    ; write = msg
                    }
                  in
                  let transitions_without_current_transi : Types.transition list =
                    List.filter
                      (fun (el : Types.transition) -> not (action_head == el.read))
                      state.transitions
                  in
                  let updated_current_state : Types.state =
                    { id = state_id
                    ; transitions =
                        List.rev_append
                          transitions_without_current_transi
                          [ new_transition ]
                    }
                  in
                  let machine_without_current_state : Types.machine =
                    List.filter (fun (el : Types.state) -> not (state_id == el.id)) machine
                  in
                  List.rev_append
                    machine_without_current_state
                    [ updated_current_state ]
                | h :: t -> add_move h t transi.to_state machine)
              | Some transi -> (
                match action_tail with
                | [] ->
                  (* no tail, write not empty *)
                  (* 2 fully identical action sequences *)
                  print_string "Syntax error on line ";
                  print_int count;
                  print_endline "";
                  print_endline
                    "This full action sequence is already present";
                  exit 3
                | h :: t -> add_move h t transi.to_state machine)))
          | _ ->
            (* requested action (head) has no keybind (2nd+)*)
            print_string "Syntax error on line ";
            print_int count;
            print_string " action: ";
            print_string action_head;
            print_endline " has no corresponding keybind";
            exit 3
        in
        (* state list *)
        let updated_machine : Types.machine =
          add_move head tail 0 accum.machine
        in
        parse
          gmr_file
          { keyconfig = accum.keyconfig; machine = updated_machine }
          Movelist
          (count + 1)
      | head :: tail ->
        (* requested action (head) has no keybind (1st)*)
        print_string "Syntax error on line ";
        print_int count;
        print_string " action: ";
        print_string head;
        print_endline " has no corresponding keybind";
        exit 3
      | _ ->
        print_string "Syntax error on line ";
        print_int count;
        print_endline "";
        exit 3)
    | _ ->
      (* movelist line fail *)
      print_string "Syntax error on line ";
      print_int count;
      print_endline "";
      exit 3)
  (* movelist parsing end*)
  | line when mode == Head ->
    (* print header *)
    (* print_endline line; *)
    parse gmr_file accum Head (count + 1)
  | _ -> parse gmr_file accum mode (count + 1)
  | exception End_of_file ->
    print_endline "finished reading the file";
    close_in gmr_file;
    accum
