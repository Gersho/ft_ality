
let pwet () =  
    Sdlevent.wait_event ();
  Format.printf "allo\n"



  let rec init () =
    Format.printf "wesh\n";
    match Sdlevent.wait_event () with
    |  KEYDOWN {keysym=k} -> 
      Format.printf "pwet\n";
      (match k with
        KEY_PLUS -> Format.printf "allo++\n"
      | KEY_MINUS -> Format.printf "allo--\n"
      | KEY_r -> Format.printf "alloRRR\n"
      | KEY_u -> Format.printf "alloUUU\n"
      | KEY_q -> Sdl.quit ()
      | _ -> init ()
      )
    | _ -> init ()


  if Sdlevent.has_event () then begin
    match Sdlevent.wait_event () with
    | KEYDOWN _ -> Format.printf "DOWN";
    | KEYUP _ -> Format.printf "UP";
    | _ -> Format.printf "???";
  end;
  Format.printf "allo\n"


let  handle_events () =
  let open Sdlevent in
  let open Sdlkey in
  let open Sdlmouse in
  (* pump (); *)
  wait_event ();
  (* match poll () with *)
  match poll () with
  | Some ( KEYDOWN { keysym = KEY_q } ) -> Sdl.quit ()
  | Some ( KEYDOWN { keysym = KEY_UP } ) -> Format.printf "allo++\n"
  | Some ( KEYDOWN { keysym = KEY_DOWN } ) -> Format.printf "allo++\n"
  | Some ( KEYDOWN { keysym = KEY_LEFT } ) -> Format.printf "allo++\n"
  | Some ( KEYDOWN { keysym = KEY_RIGHT } ) -> Format.printf "allo++\n"
  | _ -> Sdltimer.delay 1000



 let sdl_init () =
  Sdl.init [`EVERYTHING];
  Sdlevent.enable_events Sdlevent.all_events_mask

(* wait a key ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
	    | Sdlevent.KEYDOWN _ -> Format.printf "allo++\n"
      | _ -> wait_key ()

let main () = sdl_init() ; wait_key()

let main () =
  Sdl.init [`VIDEO];
  Format.printf "powered by libcaca\n";
  let test =  Sdlevent.make_mask [KEYDOWN_EVENT; KEYUP_EVENT] in
  Sdlevent.enable_events test;
  Sdlevent.pump ();
  handle_events ();
  Sdl.quit ()

    (* init() *)
    (* pwet();
    Sdl.quit () *)

(* let _ = main () *)


(* let () = print_endline "Hello, World!" *)


let handle_events () =
  let open Sdlevent in
  let open Sdlkey in
  let open Sdlmouse in
  pump ();
  match poll () with
  | Some ( MOUSEBUTTONDOWN { mbe_button = BUTTON_LEFT }
         | KEYDOWN { keysym = KEY_q } ) -> raise Exit
  | Some ( MOUSEBUTTONDOWN { mbe_button = BUTTON_RIGHT } ) -> raise Reset
  | Some ( MOUSEMOTION { mme_x ; mme_y } ) ->
    ship_pos := (float mme_x, float mme_y)
  | Some ( KEYDOWN { keysym = KEY_UP } ) -> move_y (-10.)
  | Some ( KEYDOWN { keysym = KEY_DOWN } ) -> move_y (10.)
  | Some ( KEYDOWN { keysym = KEY_LEFT } ) -> move_x (-10.)
  | Some ( KEYDOWN { keysym = KEY_RIGHT } ) -> move_x (10.)
  | _ -> ()



  open Sdlevent
open Sdlkey

let rec wait_for_escape () =
    match wait_event () with
    | KEYDOWN {keysym=KEY_ESCAPE} ->
        print_endline "You pressed escape! The fun is over now."
    | event ->
        print_endline (string_of_event event);
        wait_for_escape ()

let main () =
    Sdl.init [`VIDEO];
    at_exit Sdl.quit;
    ignore (Sdlvideo.set_video_mode 200 200 []);
    wait_for_escape ()

let _ = main ()




(* https://stackoverflow.com/questions/7579135/problem-handling-keyboard-with-opengl-and-sdl-in-ocaml *)


################################################################################
################################################################################
################################################################################
current:


(* let key_handler k = match k with
| {Sdlevent.keysym=Sdlkey.KEY_ESCAPE} ->
    print_endline "I'll miss you so much..." ;
| {Sdlevent.keysym=Sdlkey.KEY_SPACE} ->
  print_endline "UP" ;
| {Sdlevent.keysym=Sdlkey.KEY_DOWN} ->
  print_endline "DOWN";   
| _ -> ()


let rec wait_for_input () = 
  Sdlevent.wait();
  match Sdlevent.poll () with
    | Some (Sdlevent.KEYDOWN k) ->
  print_endline "event!"; key_handler k
    | None | _ -> print_endline "nothing!(should not print)";
  wait_for_input () *)


  let rec wait_key () =
    let e = Sdlevent.wait_event () in
      match e with
        | Sdlevent.KEYDOWN _ -> Format.printf "allo++\n"
        | _ -> wait_key ()
  
  
  let main () = 
    Sdl.init [`VIDEO];
    at_exit Sdl.quit;
    let mask =  Sdlevent.make_mask [KEYDOWN_EVENT; KEYUP_EVENT] in
    Sdlevent.enable_events mask;
    (* wait_for_input () *)
    wait_key ()
  
  let _ = main ()