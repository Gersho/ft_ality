type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

type full_config = Types.full_config

type state = Types.state

type transition = Types.transition

type key = Types.key

type machine = Types.machine

let syntax_error line =
  print_string "Syntax error on line ";
  print_int line;
  print_endline "";
  exit 3


let parse_key (accum : full_config) (line : int) = function
  | [input; output]
    when List.exists (fun (key : key) -> String.equal key.input_string input) accum.keyconfig ->
      print_string "Duplicated key ";
      print_string input;
      print_string " on line ";
      print_int line;
      print_endline "";
      exit 3
  | [input; output] when String.length input > 0 && String.length output > 0 ->
      ({
         keyconfig =
           List.rev_append accum.keyconfig [{ input_string = input; output_string = output }];
         machine = accum.machine;
       }
       : full_config)
  | _ ->
      print_string "Syntax error on line ";
      print_int line;
      print_endline "";
      exit 3


let add_move move_name (line : int) actions (accum : full_config) (state : state) =
  let key_exists output =
    List.exists (fun (el : key) -> String.equal el.output_string output) accum.keyconfig
  in
  let rec add_move_rec actions (state : state) machine =
    let get_max_id = List.fold_left (fun a (b : state) -> max a b.id) 0 machine
    and get_state id : state = List.find (fun (el : state) -> el.id == id) machine
    and update_state (machine : machine) (state : state) : machine =
      List.rev_map (fun (a : state) -> if a.id == state.id then state else a) machine
    and update_state_transition (state : state) (transition : transition) : state =
      {
        id = state.id;
        transitions =
          List.rev_map
            (fun (a : transition) -> if a.read == transition.read then transition else a)
            state.transitions;
      }
    and with_state (state : state) (machine : machine) : machine = List.rev_append machine [state]
    and with_transition (state : state) (transition : transition) : state =
      { id = state.id; transitions = List.rev_append state.transitions [transition] }
    and new_transition read to_state write : transition = { read; to_state; write } in
    match actions with
    | head :: tail when key_exists head -> (
        match
          List.find_opt (fun (el : transition) -> String.equal head el.read) state.transitions
        with
        | None ->
            (* state has no transition for action_head *)
            let new_state : state = { id = get_max_id + 1; transitions = [] }
            and msg = if List.length tail == 0 then move_name else "" in
            new_transition head new_state.id msg
            |> with_transition state
            |> update_state machine
            |> with_state new_state
            |> add_move_rec tail new_state
        | Some transition when String.equal transition.write "" -> (
            match tail with
            | [] ->
                new_transition transition.read transition.to_state move_name
                |> update_state_transition state
                |> update_state machine
            | _ -> add_move_rec tail (get_state transition.to_state) machine)
        | Some transition -> (
            match tail with
            | [] ->
                (* no tail, write not empty *)
                (* 2 fully identical action sequences *)
                print_string "Syntax error on line ";
                print_int line;
                print_endline "";
                print_endline "This full action sequence is already present";
                exit 3
            | h :: t ->
                add_move_rec tail (get_state transition.to_state) machine))
    | [] -> machine
    | _ ->
        print_string "Syntax error on line ";
        print_int line;
        print_string " action: ";
        print_string (List.nth actions 0);
        print_endline " has no corresponding keybind";
        exit 3
  in
  add_move_rec actions state accum.machine


let parse_transition (accum : full_config) (line : int) = function
  | [actions_as_string; move_name] when String.length move_name > 0 -> (
      match String.split_on_char '-' actions_as_string with
      | [] -> syntax_error line
      | actions ->
          ({
             keyconfig = accum.keyconfig;
             machine =
               List.find (fun (e : state) -> e.id == 0) accum.machine
               |> add_move move_name line actions accum;
           }
           : full_config))
  | _ -> syntax_error line


let rec parse (ic : in_channel) (mode : parsing_mode) count (accum : full_config) =
  match String.trim (input_line ic) with
  | "" -> parse ic mode (count + 1) accum (* skip empty lines *)
  | "@keyconfig" -> parse ic Keyconfig (count + 1) accum
  | "@movelist" -> parse ic Movelist (count + 1) accum
  | l when mode == Keyconfig ->
      String.split_on_char ':' l |> parse_key accum count |> parse ic mode (count + 1)
  | l when mode == Movelist ->
      String.split_on_char ':' l |> parse_transition accum count |> parse ic mode (count + 1)
  | l when mode == Head ->
      (* print_endline l; *)
      parse ic Head (count + 1) accum
  | _ -> parse ic mode (count + 1) accum
  | exception End_of_file ->
      close_in ic;
      accum
