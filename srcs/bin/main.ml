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
      | "" -> parser accum mode (count + 1);
      | line when String.equal line "@keyconfig" -> begin
        parser accum Keyconfig (count + 1); end;
      | line when String.equal line "@movelist" -> begin
        parser accum Movelist (count + 1); end;
      | line when mode == Keyconfig -> begin
        (* keyconfig parsing
        -split on :
        -check for 2 non empty elem
        -check accum if input not already present
        -> add to accum or exit syntax error *)
        (* let key_as_list = String.split_on_char ':' line in *)
        let duplicate_check  input elem = String.equal elem.input_string input in 
        match String.split_on_char ':' line with
        | input :: output when
            (List.length output == 1) &&
            (String.length input > 0) &&
            (String.length (List.nth output 0) > 0) &&
            not (List.exists (duplicate_check input) accum.keyconfig)
            -> begin
            let new_rec: key = { input_string = input; output_string = List.nth output 0;} in
            parser { keyconfig = (List.rev_append accum.keyconfig [new_rec]) ; machine = accum.machine ;} Keyconfig (count + 1)
          end;
        | _ -> begin
            print_string "Syntax error on line ";
            print_int count;
            print_endline "";
            exit 3;
          end;
      end;(* keyconfig parsing *)




      (* movelist parsing *)
      | line when mode == Movelist -> begin
        (* print_endline "in movelist mode"; *)

        (* 
        -split on :
        -check for 2 non empty elem
            first elem:
            - check if - is present
                -> if not, it is 1 action move
            -split on -

        ...check if  *)


        (* let move_as_list = String.split_on_char ':' line in
        match move_as_list *)

        parser accum Movelist (count + 1);
      end;(* movelist parsing *)





      | _ -> begin
        parser accum Head (count + 1); 
      end;
      | exception End_of_file -> (
        print_endline "finished reading the file";
        close_in gmr_file;
        accum)
    in
    (* let result: full_config = parser full_config, parsing_mode, line_count *)
    let result = parser { keyconfig = [] ; machine = [];} Head 1
    in


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
    List.iter print_keyconf result.keyconfig;

    print_endline "movelist: ";
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
    List.iter print_state result.machine;

  with e ->
    raise e


let () = main ()