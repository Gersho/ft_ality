type full_config = Types.full_config

type state = Types.state

type transition = Types.transition

type key = Types.key

let rec get_input () : string =
  match Sdl.Event.poll_event () with
  | Some (Sdl.Event.KeyDown evt) -> Sdlkeycode.to_string evt.keycode
  | Some (Sdl.Event.Quit _) -> exit 0
  | _ -> get_input ()


let rec machine_rec (config : full_config) debug state =
  let rec process_input state = function
    | Some (input : key) -> (
        let transitions = (List.nth config.machine state).transitions in
        List.find_opt
          (fun (el : transition) -> String.equal input.output_string el.read)
          transitions
        |> function
        | Some transi -> (
            if debug then Debug.print_has_transition state transi;
            match transi.write with
            | "" -> machine_rec config debug transi.to_state
            | str ->
                print_endline transi.write;
                machine_rec config debug transi.to_state)
        | None ->
            if debug then Debug.print_no_transition input.output_string state;
            if state == 0 then machine_rec config debug 0 else process_input 0 (Some input))
    | None -> machine_rec config debug state
  in
  List.find_opt (fun (el : key) -> String.equal el.input_string (get_input ())) config.keyconfig
  |> process_input state


let run (config : full_config) (debug : bool) = machine_rec config debug 0
