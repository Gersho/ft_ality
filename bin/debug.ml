(* print_key_mappings should be separate from other print, it must be printed even without debug *)
let print_key_mappings (keyconfig : Types.key list) =
  let print_keyconf (key : Types.key) =
    print_string "key.input: ";
    print_string key.input_string;
    print_string " -> key.output: ";
    print_endline key.output_string
  in
  print_endline "";
  print_endline "#### Key Configuration ####";
  print_endline "";
  List.iter print_keyconf keyconfig;
  print_endline ""


let print_machine (machine : Types.machine) =
  print_endline "";
  print_endline "#### Machine States ####";
  print_endline "";
  let print_transition (elem : Types.transition) =
    print_string "     read: ";
    print_string elem.read;
    print_string " to_state: ";
    print_int elem.to_state;
    print_string " write: ";
    print_endline elem.write
  in
  let print_state (elem : Types.state) =
    print_string "State id: ";
    print_int elem.id;
    print_endline "";
    List.iter print_transition elem.transitions
  in
  List.iter print_state machine


let print_has_transition (current_state : int) (transi : Types.transition) =
  print_string "State: ";
  print_string "\027[34m";
  print_int current_state;
  print_string "\027[37m";
  print_string "\027[32m has a follow-up \027[37m for action: ";
  print_string "\027[33m";
  print_string transi.read;
  print_string "\027[37m";
  print_string " pointing to state ";
  print_string "\027[34m";
  print_int transi.to_state;
  print_string "\027[37m";
  match transi.write with
  | "" -> print_endline " with no move, printing nothing"
  | str ->
      print_string " with a move, printing ";
      print_endline transi.write


let print_no_transition (input : string) (current_state : int) =
  print_string "State: ";
  print_string "\027[34m";
  print_int current_state;
  print_string "\027[37m";
  print_string "\027[31m has no follow-up \027[37m for action: ";
  print_string "\027[33m";
  print_string input;
  print_string "\027[37m";
  if current_state == 0 then
    print_endline " staying in state 0"
  else (
    print_endline " -> going back to state \027[34m0\027[37m and reprocess action";
    print_string " -> ")
