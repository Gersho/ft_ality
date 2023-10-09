(* allowed modules Pervasives Sys List String Sdl Sdlevent Sdlkey *)

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
      Parser.parse gmr_file { keyconfig = []; machine = [{ id = 0; transitions = [] }] } Head 1
    in
    let final_parsed : Types.full_config =
      let machine_sorted =
        List.sort (fun (a : Types.state) (b : Types.state) -> compare a.id b.id) result.machine
      in
      { keyconfig = result.keyconfig; machine = machine_sorted }
    in

    (* debug print *)
    print_endline "debug print: ";
    print_endline "keyconf: ";
    let print_keyconf (elem : Types.key) =
      print_string "key.input: ";
      print_string elem.input_string;
      print_string " key.output: ";
      print_endline elem.output_string
    in
    List.iter print_keyconf final_parsed.keyconfig;
    print_endline "machine: ";
    let print_transition (elem : Types.transition) =
      print_string "     read: ";
      print_string elem.read;
      print_string " to_state: ";
      print_int elem.to_state.id;
      print_string " write: ";
      print_endline elem.write
    in
    let print_state (elem : Types.state) =
      print_string "id: ";
      print_int elem.id;
      print_endline "";
      List.iter print_transition elem.transitions
    in
    List.iter print_state final_parsed.machine;

    print_endline "#### START MACHINE ####";
    let hardcoded_test =
      ["2"; "3"; "6"; "MP"; "LP"; "LP"; "1"; "2"; "3"; "6"; "1"]
      (* [ "1"; "2"; "2"; "1"; "1" ] *)
    in
    let rec machine_test state count =
      (* print_string "current state: "; print_int state; print_string " next input is: ";
         print_endline (List.nth hardcoded_test count); *)
      if count < List.length hardcoded_test then
        let input = List.nth hardcoded_test count in
        let a = (List.nth final_parsed.machine state).transitions in
        match List.find_opt (fun (el : Types.transition) -> String.equal input el.read) a with
        | None ->
            print_string "input: ";
            print_string input;
            print_string " has no follow-up in state: ";
            print_int state;
            print_endline " -> going back to state 0";
            machine_test 0 (count + 1)
        | Some transi -> (
            print_string "input: ";
            print_string input;
            print_string " has a follow-up in state: ";
            print_int state;
            print_string " pointing to state ";
            print_int transi.to_state.id;
            print_endline "";

            match transi.write with
            | "" ->
                print_endline "<not a move, writing nothing>";
                machine_test transi.to_state.id (count + 1)
            | str ->
                print_string "move found, writing: ";
                print_endline transi.write;
                machine_test transi.to_state.id (count + 1))
    in
    machine_test 0 0
  with
  | e -> raise e


let () = main ()
