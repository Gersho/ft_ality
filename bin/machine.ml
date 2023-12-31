type full_config = Types.full_config

type state = Types.state

type transition = Types.transition

type key = Types.key

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
            if debug then Debug.print_no_transition state input.output_string;
            if state == 0 then machine_rec config debug 0 else process_input 0 (Some input))
    | None -> machine_rec config debug state
  in
  let next_input = Input.get_input () in
  List.find_opt (fun (el : key) -> String.equal el.input_string next_input) config.keyconfig
  |> process_input state


let run (config : full_config) (debug : bool) = machine_rec config debug 0
