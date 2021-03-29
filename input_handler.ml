let key_checker () =
  while true do
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
  done
