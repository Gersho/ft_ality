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
    let rec parser (accum :full_config) mode = 

      match String.trim (input_line (gmr_file)) with
      | line when String.equal line "@keyconfig" -> begin
        print_endline "found keyconfig";
        parser accum Keyconfig;
      end;
      | line when String.equal line "@movelist" -> begin
        print_endline "found movelist";
        parser accum Movelist;
      end;

      (* keyconfig parsing *)
      | line when mode == Keyconfig -> begin
        (* print_endline "in keyconf mode"; *)

        (* -check is : is present
        -split on :
        -check for 2 non empty elem
        -check accum if input not already present
        -> add to accum or exit syntax error *)
        
        (* parser { keyconfig = [] ; machine = accum.machine ;} Head  *)
        parser accum Keyconfig;
      end;(* keyconfig parsing *)


      (* movelist parsing *)
      | line when mode == Movelist -> begin
        (* print_endline "in movelist mode"; *)

        -check is : is present
        -split on :
        -check for 2 non empty elem
            first elem:
            - check if - is present
                -> if not, it is 1 action move
            -split on -

        ...check if 


        parser accum Movelist;
      end;(* movelist parsing *)





      | line -> begin
        (* print_endline "in head mode"; *)
        parser accum Head; 
      end;
      | exception End_of_file -> (
        print_endline "finished reading the file";
        close_in gmr_file;
        accum)



    in
    (* let result = parser ([], (_ , [])) Head  *)
    let result = parser { keyconfig = [] ; machine = [];} Head 
    in


    (* debug print *)
    print_string "line count: ";
    (* print_int (List.length (result)); *)
    print_endline "";
    (* let print_list (elem) = print_endline elem in
    List.iter print_list result; *)


  with e ->
    raise e


let () = main ()