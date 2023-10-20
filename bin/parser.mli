type parsing_mode =
  | Head
  | Keyconfig
  | Movelist

val parse : in_channel -> Types.full_config
