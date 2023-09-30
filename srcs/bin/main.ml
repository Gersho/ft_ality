(* allowed modules Pervasives Sys List String Sdl Sdlevent Sdlkey *)

type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

type transition =
  { read : string
  ; to_state : int
  ; write : string
  }

type state =
  { id : int
  ; transitions : transition list
  }

type key =
  { input_string : string
  ; output_string : string
        (* temporary have to see how events work and how we get keysym/keycode
           info input_keysym : Sdlkey.t; <- have to see how events work and *)
  }

type full_config =
  { keyconfig : key list
  ; machine : state list
  }

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
    let rec parser (accum : full_config) mode count =
      let parse_key (accum : full_config) line count =
        let duplicate_check input elem = String.equal elem.input_string input in
        match String.split_on_char ':' line with
        | input :: output
          when List.length output == 1
               && String.length input > 0
               && String.length (List.nth output 0) > 0
               && not (List.exists (duplicate_check input) accum.keyconfig) ->
          let new_rec : key =
            { input_string = input; output_string = List.nth output 0 }
          in
          parser
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
      | "" -> parser accum mode (count + 1)
      (* switch parsing mode *)
      | line when String.equal line "@keyconfig" ->
        parser accum Keyconfig (count + 1)
      | line when String.equal line "@movelist" ->
        parser accum Movelist (count + 1)
      (* keyconfig parsing *)
      | line when mode == Keyconfig -> parse_key accum line count
      (* movelist parsing *)
      | line when mode == Movelist -> (
        match String.split_on_char ':' line with
        | actions_as_string :: move_name
          when List.length move_name == 1
               && String.length actions_as_string > 0
               && String.length (List.nth move_name 0) > 0 -> (
          let is_valid_key action elem =
            String.equal elem.output_string action
          in
          match String.split_on_char '-' actions_as_string with
          | head :: tail
            when String.length head > 0
                 && List.exists (is_valid_key head) accum.keyconfig ->
            let rec add_move action_head action_tail state_id machine =
              match action_head with
              | str
                when String.length str > 0
                     && List.exists (is_valid_key str) accum.keyconfig -> (
                let rec get_new_id acc list =
                  match list with
                  | [] -> acc + 1
                  | hd :: tl ->
                    if hd.id > acc then get_new_id hd.id tl
                    else get_new_id acc tl
                in
                match List.find_opt (fun el -> state_id == el.id) machine with
                | None ->
                  print_string "Something went very wrong trying to parse line ";
                  print_int count;
                  print_endline " (should never display)";
                  exit 4
                | Some state -> (
                  match
                    List.find_opt
                      (fun el -> String.equal action_head el.read)
                      state.transitions
                  with
                  | None -> (
                    (* state has no transition for action_head *)
                    let new_id = get_new_id 0 machine in
                    let msg =
                      if List.length action_tail == 0 then List.nth move_name 0
                      else ""
                    in
                    let new_transition =
                      { read = action_head; to_state = new_id; write = msg }
                    in
                    let updated_current_state =
                      { id = state_id
                      ; transitions =
                          List.rev_append state.transitions [ new_transition ]
                      }
                    in
                    let machine_without_current_state =
                      List.filter (fun el -> not (state_id == el.id)) machine
                    in
                    let machine_with_updated_state =
                      List.rev_append
                        machine_without_current_state
                        [ updated_current_state ]
                    in
                    let new_state = { id = new_id; transitions = [] } in
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
                      let new_transition =
                        { read = transi.read
                        ; to_state = transi.to_state
                        ; write = msg
                        }
                      in
                      let transitions_without_current_transi =
                        List.filter
                          (fun el -> not (action_head == el.read))
                          state.transitions
                      in
                      let updated_current_state =
                        { id = state_id
                        ; transitions =
                            List.rev_append
                              transitions_without_current_transi
                              [ new_transition ]
                        }
                      in
                      let machine_without_current_state =
                        List.filter (fun el -> not (state_id == el.id)) machine
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
            let updated_machine : state list =
              add_move head tail 0 accum.machine
            in
            parser
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
        parser accum Head (count + 1)
      | _ -> parser accum mode (count + 1)
      | exception End_of_file ->
        print_endline "finished reading the file";
        close_in gmr_file;
        accum
    in
    let result =
      parser
        { keyconfig = []; machine = [ { id = 0; transitions = [] } ] }
        Head
        1
    in
    let final_parsed =
      let machine_sorted =
        List.sort (fun a b -> compare a.id b.id) result.machine
      in
      { keyconfig = result.keyconfig; machine = machine_sorted }
    in

    (* debug print *)
    print_endline "debug print: ";
    print_endline "keyconf: ";
    let print_keyconf (elem : key) =
      print_string "key.input: ";
      print_string elem.input_string;
      print_string " key.output: ";
      print_endline elem.output_string
    in
    List.iter print_keyconf final_parsed.keyconfig;
    print_endline "machine: ";
    let print_transition (elem : transition) =
      print_string "     read: ";
      print_string elem.read;
      print_string " to_state: ";
      print_int elem.to_state;
      print_string " write: ";
      print_endline elem.write
    in
    let print_state (elem : state) =
      print_string "id: ";
      print_int elem.id;
      print_endline "";
      List.iter print_transition elem.transitions
    in
    List.iter print_state final_parsed.machine;

    print_endline "#### START MACHINE ####";
    let hardcoded_test =
      [ "2"; "3"; "6"; "MP"; "LP"; "LP"; "1"; "2"; "3"; "6"; "1" ]
    in
    let rec machine_test state count =
      (* print_string "current state: "; print_int state; print_string " next
         input is: "; print_endline (List.nth hardcoded_test count); *)
      let input = List.nth hardcoded_test count in
      let a = (List.nth final_parsed.machine state).transitions in
      match List.find_opt (fun el -> String.equal input el.read) a with
      | None ->
        print_string "input: ";
        print_string input;
        print_string " has no follow-up in state: ";
        print_int state;
        print_endline " -> going back to state 0";
        machine_test 0 (count + 1)
      | Some transi ->
        (print_string "input: ";
         print_string input;
         print_string " has a follow-up in state: ";
         print_int state;
         print_string " pointing to state ";
         print_int transi.to_state;
         print_endline "";

         match transi.write with
         | "" ->
           print_endline "<not a move, writing nothing>";
           machine_test transi.to_state (count + 1)
         | str ->
           print_string "move found, writing: ";
           print_endline transi.write;
           machine_test transi.to_state (count + 1));
        machine_test state (count + 1)
    in
    machine_test 0 0
  with e -> raise e

let () = main ()
