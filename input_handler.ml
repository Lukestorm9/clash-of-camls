(*need input handling for wasd*)

let key_checker () =
  if Sdlkey.is_key_pressed KEY_w then print_endline "w was pressed"
  else if Sdlkey.is_key_pressed KEY_a then print_endline "a was pressed"
  else if Sdlkey.is_key_pressed KEY_d then print_endline "d was pressed"
  else if Sdlkey.is_key_pressed KEY_s then print_endline "s was pressed"
  else print_endline "not pressed"
