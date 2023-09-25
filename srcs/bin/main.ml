



let main () =
  Sdl.init [`VIDEO];
  Format.printf "powered by libcaca";
  let test =  Sdlevent.make_mask [KEYDOWN_EVENT; KEYUP_EVENT] in
  Sdlevent.enable_events test;
  Sdl.quit ()

let _ = main () 


(* let () = print_endline "Hello, World!" *)