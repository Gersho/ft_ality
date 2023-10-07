type transition =
  { read : string
  ; to_state : int
  ; write : string
  }

type state =
  { id : int
  ; transitions : transition list
  }

type key =
  { input_string : string
  ; output_string : string
        (* temporary have to see how events work and how we get keysym/keycode
           info input_keysym: Sdlkey.t; <- have to see how events work and *)
  }

type machine = state list

type full_config =
  { keyconfig : key list
  ; machine : machine
  }
