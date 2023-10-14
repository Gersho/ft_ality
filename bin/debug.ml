(* keyconfig should be separate from other print, it must be printed even without debug *)

let print_key_mappings (keyconfig: Types.keyconfig)=
  let print_keyconf (key : Types.key) =
    print_string "key.input: ";
    print_string key.input_string;
    print_string " -> key.output: ";
    print_endline key.output_string
  in
  List.iter print_keyconf keyconfig


(* should not ne printed if no debug *)
let print_machine (machine: Types.machine)=
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
  List.iter print_state machine




let print_transition (input: String) (current_state: int)(to_state: int option) = 
  print_string "State: ";
  print_int current_state;
  match to_state with
  |Some num -> (
    print_string " has a follow-up for input: ";
    print_string input;
    print_string " pointing to state ";
    print_int to_state;
    print_endline "";
  )
  |None -> (
    print_string " has no follow-up for input: ";
    print_string input;
    print_endline " -> going back to state 0 and reprocess input";
  )

