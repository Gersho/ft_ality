type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

val parse : in_channel -> parsing_mode -> int -> Types.full_config -> Types.full_config
