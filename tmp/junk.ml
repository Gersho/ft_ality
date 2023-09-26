let get_words =
    let re = Str.regexp "[ \t]+" in
    fun s ->
        Str.split re s

let extract ic =
    let rec loop accum =
        match input_line ic with
        | line ->
            (match get_words line with
            | w1 :: _ ::  w3 :: _ :: w5 :: _ ->
                loop ((w1, w3, w5) :: accum)
            | _ -> loop accum
            )
        | exception End_of_file -> List.rev accum
     in
     loop []


     # let rec append a b =
      match a with
      | [] -> b
      | h :: t -> h :: append t b;;
    val append : 'a list -> 'a list -> 'a list = <fun>