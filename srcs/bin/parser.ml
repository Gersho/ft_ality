type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

type full_config = Types.full_config

type state = Types.state

type transition = Types.transition

type key = Types.key

type machine = Types.machine

let rec parse (ic : in_channel) (accum : full_config) mode count =
  let parse_key =
    let is_valid_to_add (input : string) (output : string) =
      String.length input > 0
      && String.length output > 0
      && not
           (List.exists
              (fun (key : key) -> String.equal key.input_string input)
              accum.keyconfig)
    and append_key (input : string) (output : string) : full_config =
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
          (fun (el : key) -> String.equal el.output_string output)
          accum.keyconfig
      in
      match String.split_on_char '-' actions_as_string with
      | [] ->
        print_string "Syntax error on line ";
        print_int count;
        print_endline "";
        exit 3
      | actions ->
        let new_config : full_config =
          let rec add_move actions (state : state) machine =
            let get_state id = List.find (fun (s : state) -> s.id == id) machine
            and get_max_id =
              List.fold_left (fun a (b : state) -> max a b.id) 0 machine
            and update_state (machine : machine) (state : state) : machine =
              List.rev_map
                (fun (a : state) -> if a.id == state.id then state else a)
                machine
            and update_state_transition (state : state)
                (transition : transition) : state =
              { id = state.id
              ; transitions =
                  List.rev_map
                    (fun (a : transition) ->
                      if a.read == transition.read then transition else a)
                    state.transitions
              }
            and with_state (state : state) (machine : machine) : machine =
              List.rev_append machine [ state ]
            and with_transition (state : state) (transition : transition) :
                state =
              { id = state.id
              ; transitions = List.rev_append state.transitions [ transition ]
              }
            and new_transition read to_state write : transition =
              { read; to_state; write }
            in
            match actions with
            | head :: tail when key_exists head -> (
              match
                List.find_opt
                  (fun (el : transition) -> String.equal head el.read)
                  state.transitions
              with
              | None ->
                (* state has no transition for action_head *)
                let new_state : state =
                  { id = get_max_id + 1; transitions = [] }
                and msg = if List.length tail == 0 then move_name else "" in
                new_transition head new_state.id msg
                |> with_transition state |> update_state machine
                |> with_state new_state |> add_move tail new_state
              | Some transition when String.equal transition.write "" -> (
                match tail with
                | [] ->
                  new_transition transition.read transition.to_state move_name
                  |> update_state_transition state
                  |> update_state machine
                | _ -> add_move tail (get_state transition.to_state) machine)
              | Some transition -> (
                match tail with
                | [] ->
                  (* no tail, write not empty *)
                  (* 2 fully identical action sequences *)
                  print_string "Syntax error on line ";
                  print_int count;
                  print_endline "";
                  print_endline "This full action sequence is already present";
                  exit 3
                | h :: t ->
                  add_move tail (get_state transition.to_state) machine))
            | [] -> machine
            | _ ->
              print_string "Syntax error on line ";
              print_int count;
              print_string " action: ";
              print_string (List.nth actions 0);
              print_endline " has no corresponding keybind";
              exit 3
          in
          { keyconfig = accum.keyconfig
          ; machine =
              add_move
                actions
                (List.find (fun (e : state) -> e.id == 0) accum.machine)
                accum.machine
          }
        in
        parse ic new_config Movelist (count + 1))
    | _ ->
      print_string "Syntax error on line ";
      print_int count;
      print_endline "";
      exit 3
  in
  match String.trim (input_line ic) with
  | "" -> parse ic accum mode (count + 1) (* skip empty lines *)
  | "@keyconfig" -> parse ic accum Keyconfig (count + 1)
  | "@movelist" -> parse ic accum Movelist (count + 1)
  | l when mode == Keyconfig -> parse_key (String.split_on_char ':' l)
  | l when mode == Movelist -> parse_transition (String.split_on_char ':' l)
  | l when mode == Head ->
    (* print_endline l; *)
    parse ic accum Head (count + 1)
  | _ -> parse ic accum mode (count + 1)
  | exception End_of_file ->
    close_in ic;
    accum
