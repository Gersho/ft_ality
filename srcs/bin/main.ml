(* allowed modules
Pervasives Sys List String Sdl Sdlevent Sdlkey *)

type parsing_mode = Head | Keyconfig | Movelist

type transition =
{
  read : string;
  to_state : int;
  write : string;
}

type state = 
{
  id : int;
  transitions : transition list;
}

type key =
{
  input_string : string;
  output_string : string;
(* temporary 
  have to see how events work and how we get keysym/keycode info
  input_keysym : Sdlkey.t; <- have to see how events work and *)
}

type full_config =
{
  keyconfig : key list;
  machine : state list;
}

let  main () = 
    (* Array forbiden module ?! *)
  (* TODO complete arg handling (help, debug, syntax) *)
  if Array.length Sys.argv < 2 then begin
    print_endline "Wrong amount of arguments";
    exit 1;
  end;
  print_string "Opening file: ";
  print_endline Sys.argv.(1);
  try
    let gmr_file = open_in Sys.argv.(1) in
    let rec parser (accum :full_config) mode count = 
      match String.trim (input_line (gmr_file)) with
      (* skip empty lines *)
      | "" -> parser accum mode (count + 1);
      (* switch parsing mode *)
      | line when String.equal line "@keyconfig" -> begin parser accum Keyconfig (count + 1); end;
      | line when String.equal line "@movelist" -> begin parser accum Movelist (count + 1); end;
      (* keyconfig parsing *)
      | line when mode == Keyconfig ->
        begin
          let duplicate_check  input elem = String.equal elem.input_string input in 
          match String.split_on_char ':' line with
          | input :: output when
            (List.length output == 1) &&
            (String.length input > 0) &&
            (String.length (List.nth output 0) > 0) &&
            not (List.exists (duplicate_check input) accum.keyconfig)
            ->
            begin
              let new_rec: key = { input_string = input; output_string = List.nth output 0;} in
              parser { keyconfig = (List.rev_append accum.keyconfig [new_rec]) ; machine = accum.machine ;} Keyconfig (count + 1)
            end;
          | _ ->
            begin
              print_string "Syntax error on line ";
              print_int count;
              print_endline "";
              exit 3;
            end;
        end;





        (* | line when mode == Movelist ->
          parser accum Movelist (count + 1) *)

      (* movelist parsing *)

      | line when mode == Movelist ->
        begin
          match String.split_on_char ':' line with
          | actions_as_string :: move_name when
            (List.length move_name == 1) &&
            (String.length actions_as_string > 0) &&
            (String.length (List.nth move_name 0) > 0)
            ->
            begin
              (* print_string "actions : ";
              print_string actions_as_string;
              print_string " name : ";
              print_string (List.nth move_name 0);
              print_endline ""; *)
              let is_valid_key action elem = begin String.equal elem.output_string action end; in
              match String.split_on_char '-' actions_as_string with
              | head :: tail when
                (String.length head > 0) &&
                (List.exists (is_valid_key head) accum.keyconfig)
                -> begin
                  let rec add_move action_head action_tail state_id machine =
                  begin
                    match action_head with
                    |str when 
                    (String.length str > 0) &&
                    (List.exists (is_valid_key str) accum.keyconfig)
                    ->
                    begin
                      let rec get_new_id acc list =
                        begin
                          match list with
                          | [] -> (acc + 1)
                          | hd::tl ->
                            begin
                              if hd.id > acc then (get_new_id hd.id tl) else (get_new_id acc tl)
                            end;
                        end;
                      in 
                      match List.find_opt (fun el -> state_id == el.id) machine with
                      | None -> 
                        begin
                          print_string "Something went very wrong trying to parse line ";
                          print_int count;
                          print_endline " (should never display)";
                          exit 4;
                        end;
                      | Some state ->
                        begin
                          let test = 
                            begin
                              List.find_opt (fun el ->  String.equal action_head el.read) state.transitions 
                            end; in
                          match test with
                          | None -> 
                            (* state has no transition for action_head *)
                            (* need to add new transition to updated current state *)
                            (* need to add new state to machine *)
                            begin
                              (* print_string "at L 139 ";
                              print_int state_id;
                              print_endline ""; *)
                              let new_id = get_new_id 0 machine in
                              let msg = if (List.length action_tail == 0) then (List.nth move_name 0) else "" in
                              let new_transition = { read = action_head; to_state = new_id; write = msg; } in
                              let updated_current_state = { id = state_id; transitions = (List.rev_append state.transitions [new_transition]); } in
                              let machine_without_current_state = List.filter (fun el -> not (state_id == el.id)) machine in
                              let machine_with_updated_state = List.rev_append machine_without_current_state [updated_current_state] in
                              let new_state = { id = new_id; transitions = [] } in
                              match action_tail with
                              | [] -> List.rev_append machine_with_updated_state [new_state]
                              | h::t -> add_move h t new_id (List.rev_append machine_with_updated_state [new_state])
                            end;
                          | Some transi when (String.equal transi.write "") ->
                            begin
                              match action_tail with
                              | [] ->
                                (* transition exists for action_head in current state *)
                                (* need to add new transition to updated current state *)
                                (* need to update machine with updated state *)
                                (* no msg + no tail -> set message *)
                                begin
                                  let msg = (List.nth move_name 0) in
                                  let new_transition = { read = transi.read ; to_state = transi.to_state ; write = msg; } in
                                  let transitions_without_current_transi = List.filter (fun el ->  not (action_head == el.read)) state.transitions in
                                  let updated_current_state = { id = state_id; transitions = (List.rev_append transitions_without_current_transi [new_transition]); } in
                                  let machine_without_current_state = List.filter (fun el -> not (state_id == el.id)) machine in
                                  List.rev_append machine_without_current_state [updated_current_state]
                                end;
                              | h::t -> add_move h t transi.to_state machine
                            end;
                          | Some transi ->
                            begin
                              match action_tail with
                              | [] ->
                                begin
                                  (* no tail, write not empty *)
                                  (* 2 fully identical action sequences *)
                                  print_string "Syntax error on line ";
                                  print_int count;
                                  print_endline "";
                                  print_endline "This full action sequence is already present";
                                  exit 3;
                                end;
                              | h::t -> add_move h t transi.to_state machine
                            end;
                        end;
                    end;
                    | _ -> begin
                      (* requested action (head) has no keybind *)
                      print_string "Syntax error on line ";
                      print_int count;
                      print_endline "";
                      exit 3;
                    end;
                  end;
                  in
                  (* state list *)
                  let updated_machine: state list = add_move head tail 0 accum.machine in
                  parser { keyconfig = accum.keyconfig ; machine = updated_machine ;} Movelist (count + 1)
                  (* parser accum Movelist (count + 1); *)
                end;

              
              | _ ->
                begin
                (* requested action (head) has no keybind *)
                print_string "Syntax error on line ";
                print_int count;
                print_endline "";
                exit 3;
                end;
            end;
          | _ ->
            begin
              (* movelist line fail *)
              print_string "Syntax error on line ";
              print_int count;
              print_endline "";
              exit 3;
            end;
        end;

        (* movelist parsing end*)




      | line when mode == Head -> begin
        (* print header *)
        (* print_endline line; *)
        parser accum Head (count + 1);
      end;
      | _ -> begin
        parser accum mode (count + 1); 
      end;
      | exception End_of_file -> (
        print_endline "finished reading the file";
        close_in gmr_file;
        accum)
    in
    (* let result: full_config = parser full_config, parsing_mode, line_count *)
    let result = parser { keyconfig = [] ; machine = [ { id = 0; transitions = [] } ];} Head 1 in
    (* final sort to enable use of List.nth (and readability of debug)*)
    
    let final_parsed = 
      let machine_sorted = List.sort (fun a b -> compare a.id b.id) result.machine in
      { keyconfig = result.keyconfig; machine = machine_sorted } in

    (* debug print *)
    print_endline "debug print: ";

    print_endline "keyconf: ";
    let print_keyconf (elem: key) = 
      begin
        print_string "key.input: ";
        print_string elem.input_string;
        print_string " key.output: ";
        print_endline elem.output_string;
      end;
    in
    List.iter print_keyconf final_parsed.keyconfig;

    print_endline "machine: ";
    let print_transition (elem: transition)= 
      begin
        print_string "     read: ";
        print_string elem.read;
        print_string " to_state: ";
        print_int elem.to_state;
        print_string " write: ";
        print_endline elem.write;
      end;
    in let print_state (elem: state) =
      begin
        print_string "id: ";
        print_int elem.id;
        print_endline "";
        List.iter print_transition elem.transitions;
      end;
    in
    List.iter print_state final_parsed.machine;

  with e ->
    raise e


let () = main ()