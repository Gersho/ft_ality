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
}

and machine = state list

and full_config = {
  keyconfig: key list;
  machine: machine;
}
