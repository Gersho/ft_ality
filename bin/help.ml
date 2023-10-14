
let print_help () =
  print_endline "usage:";
  print_endline "./ft_ality --help";
  print_endline "   shows this help message";
  print_endline "./ft_ality --file-syntax";
  print_endline "   shows file syntax help";
  print_endline "./ft_ality <grammar file> [--debug]";
  print_endline "   opens the programm with the provided grammar file";
  print_endline "   --debug option for state and transition verbose info"


let print_syntax () =
  print_endline "leading/trailing whitespace are ignored";
  print_endline "empty lines are ignored";
  print_endline "section names start with a @";
  print_endline "2 sections, @keyconfig and @movelist";
  print_endline "each section is required";
  print_endline "lines before the first section are printed but ignored";
  print_endline "";
  print_endline "	@keyconfig:";
  print_endline "		symbol ":" is reserved and cannot be used as a key nor as part of an action";
  print_endline "		<key>:<action>";
  print_endline "		key and action are separated by symbol :";
  print_endline "";
  print_endline "		one <key>:<action> pair per line";
  print_endline "		section must have at least 1 <key>:<action> pair";
  print_endline "		a key cannot be repeated";
  print_endline "		an action can be repeated";
  print_endline "";
  print_endline "		<key> reserved by system cannot be used (such as media keys, super, printscreen)";
  print_endline "		<key> naming is based on (keycode)_to_string from sdlkeycode.ml";
  print_endline "			-> numeric keypad keys must be prefaced by KP_ such as";
  print_endline "				KP_2";
  print_endline "				KP_Minus";
  print_endline "			-> single letters must be upper-case";
  print_endline "			-> numbers from the character keys must be prefaced by Num such as";
  print_endline "				Num2";
  print_endline "				Num9";
  print_endline "";
  print_endline "		exemple:";
  print_endline "		a:kick";
  print_endline "		k:HK";
  print_endline "		l:LP";
  print_endline "		KP_2:2";
  print_endline "		KP_3:3";
  print_endline "		KP_6:6";
  print_endline "		q:LP";
  print_endline "		w:MP";
  print_endline "		e:HP";
  print_endline "";
  print_endline "	@movelist:";
  print_endline "		<action>-<action>-<action>:<move>";
  print_endline "		actions are separated by -";
  print_endline "		actions and move are separated by :";
  print_endline "		- and : are reserved for action (but can be used for move name)";
  print_endline "";
  print_endline "		section must have at least 1 <action>:<move> pair";
  print_endline "		the same full action sequence cannot be repeated";
  print_endline "";
  print_endline "		exemple:";
  print_endline "		2-3-6-LP:Hadouken";
  print_endline "";
  print_endline "";
  print_endline "full exemple:";
  print_endline "@keyconfig";
  print_endline "2:2";
  print_endline "3:3";
  print_endline "6:6";
  print_endline "q:LP";
  print_endline "w:MP";
  print_endline "e:HP";
  print_endline "@movelist";
  print_endline "2-3-6-LP:Hadouken"

