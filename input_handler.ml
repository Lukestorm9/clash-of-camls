(*need input handling for wasd*)
(*let key_checker () = let keys = Sdlkey.get_key_state () in
  (*print_endline (string_of_int (Sdlkey.int_of_key Sdlkey.KEY_a));*)
  (*if keys.{Sdlkey.int_of_key Sdlkey.KEY_a} != 0 then print_endline "a
  was pressed"*) if Sdlkey.is_key_pressed KEY_a then print_endline "a
  was pressed" else ()*)

let key_checker () =
  match Sdlevent.wait_event () with
  | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_ESCAPE } ->
      print_endline "esc was pressed";
      Sdl.quit ()
  | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_w } ->
      print_endline "w was pressed";
      ()
  | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_a } ->
      print_endline "a was pressed";
      ()
  | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_s } ->
      print_endline "s was pressed";
      ()
  | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_d } ->
      print_endline "d was pressed";
      ()
  | _ -> ()
