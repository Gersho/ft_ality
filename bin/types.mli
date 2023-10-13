type transition = {
  read: string;
  to_state: int;
  write: string;
}

and state = {
  id: int;
  transitions: transition list;
}

and key = {
  input_string: string;
  output_string: string;
  (* temporary have to see how events work and how we get keysym/keycode info input_keysym:
     Sdlkey.t; <- have to see how events work and *)
}

and machine = state list

and full_config = {
  keyconfig: key list;
  machine: machine;
}
