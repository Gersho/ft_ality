type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

let rec parse (ic : in_channel) (accum : Types.full_config) mode count =
  let parse_key =
    let is_valid_to_add (input : string) (output : string) =
      String.length input > 0
      && String.length output > 0
      && not
           (List.exists
              (fun (key : Types.key) -> String.equal key.input_string input)
              accum.keyconfig)
    and append_key (input : string) (output : string) : Types.full_config =
      { keyconfig =
          List.rev_append
            accum.keyconfig
            [ { input_string = input; output_string = output } ]
      ; machine = accum.machine
      }
    in
    function
    | [ input; output ] when is_valid_to_add input output ->
      parse ic (append_key input output) Keyconfig (count + 1)
    | _ ->
      print_string "Syntax error on line ";
      print_int count;
      print_endline "";
      exit 3
  and parse_transition = function
    | [ actions_as_string; move_name ]
      when String.length actions_as_string > 0 && String.length move_name > 0
      -> (
      let key_exists output =
        List.exists
          (fun (el : Types.key) -> String.equal el.output_string output)
          accum.keyconfig
      in
      match String.split_on_char '-' actions_as_string with
      | [] ->
        print_string "Syntax error on line ";
        print_int count;
        print_endline "";
        exit 3
      | actions ->
        let rec add_move actions (state : Types.state) machine =
          match actions with
          | head :: tail when key_exists head -> (
            match
              List.find_opt
                (fun (el : Types.transition) -> String.equal head el.read)
                state.transitions
            with
            | None ->
              (* state has no transition for action_head *)
              let new_state : Types.state =
                let new_id =
                  List.fold_left
                    (fun a (b : Types.state) -> max a b.id)
                    0
                    machine
                  + 1
                in
                { id = new_id; transitions = [] }
              in
              let machine =
                let state : Types.state =
                  let new_transition : Types.transition =
                    let msg = if List.length tail == 0 then move_name else "" in
                    { read = head; to_state = new_state.id; write = msg }
                  in
                  { id = state.id
                  ; transitions =
                      List.rev_append state.transitions [ new_transition ]
                  }
                in
                List.rev_append
                  (List.rev_map
                     (fun (a : Types.state) ->
                       if a.id == state.id then state else a)
                     machine)
                  [ new_state ]
              in
              add_move tail new_state machine
            | Some transi when String.equal transi.write "" -> (
              match action_tail with
              | [] ->
                let msg = move_name in
                let new_transition : Types.transition =
                  { read = transi.read
                  ; to_state = transi.to_state
                  ; write = msg
                  }
                in
                let transitions_without_current_transi : Types.transition list =
                  List.filter
                    (fun (el : Types.transition) ->
                      not (action_head == el.read))
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
                  List.filter
                    (fun (el : Types.state) -> not (state_id == el.id))
                    machine
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
                print_endline "This full action sequence is already present";
                exit 3
              | h :: t -> add_move h t transi.to_state machine))
          | [] -> machine
          | _ ->
            print_string "Syntax error on line ";
            print_int count;
            print_string " action: ";
            print_string (List.nth actions 0);
            print_endline " has no corresponding keybind";
            exit 3
        in
        let new_config : Types.full_config =
          { keyconfig = accum.keyconfig
          ; machine = add_move head tail 0 accum.machine
          }
        in
        parse ic new_config Movelist (count + 1)
      (* | head :: tail ->
         (* requested action (head) has no keybind (1st)*)
         print_string "Syntax error on line ";
         print_int count;
         print_string " action: ";
         print_string head;
         print_endline " has no corresponding keybind";
         exit 3 *))
    | _ ->
      print_string "Syntax error on line ";
      print_int count;
      print_endline "";
      exit 3
  in
  match String.trim (input_line ic) with
  | "" -> parse ic accum mode (count + 1) (* skip empty lines *)
  | l when String.equal l "@keyconfig" -> parse ic accum Keyconfig (count + 1)
  | l when String.equal l "@movelist" -> parse ic accum Movelist (count + 1)
  | l when mode == Keyconfig -> parse_key (String.split_on_char ':' l)
  | l when mode == Movelist -> parse_transition (String.split_on_char ':' l)
  | l when mode == Head ->
    (* print_endline l; *)
    parse ic accum Head (count + 1)
  | _ -> parse ic accum mode (count + 1)
  | exception End_of_file ->
    close_in ic;
    accum
