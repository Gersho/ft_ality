
let () = 
  (* Array forbiden module ?! *)
  (* TODO complete arg handling (help, debug, syntax) *)
  if Array.length Sys.argv < 2 then begin
    failwith "Wrong amount of arguments";
  end;

  Format.printf "argv : %s\n" Sys.argv.(1);
  try
    let gmr_file = open_in Sys.argv.(1) in
    let rec parser accum = 
      match input_line gmr_file with
      | line -> begin
        Format.printf "reading a line\n";
        parser (List.append accum [line]);
      end;
      | exception End_of_file -> (
        Format.printf "finished reading\n";
        accum)
    in
    let result = parser [] 
    in
    Format.printf "line count: %d\n" (List.length (result));
    (* flush stdout; *)
  with e ->
    raise e;



