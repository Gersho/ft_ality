let init () =
  try
    Sdl.init [`VIDEO];
    ignore (Sdl.Render.create_window_and_renderer ~width:0 ~height:0 ~flags:[Sdlwindow.Borderless]);
    at_exit Sdl.quit
  with
  | e -> raise e


let rec get_input () : string =
  match Sdl.Event.poll_event () with
  | Some (Sdl.Event.KeyDown evt) -> Sdlkeycode.to_string evt.keycode
  | Some (Sdl.Event.Quit _) -> exit 0
  | _ -> get_input ()
