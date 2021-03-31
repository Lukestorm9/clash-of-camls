let mutex_helper
    (world_state : Common.world_state)
    (action : Common.action) =
  Mutex.lock world_state.mutex;
  world_state.user_command := action;
  Mutex.unlock world_state.mutex

let key_checker (world_state : Common.world_state) =
  Sdlkey.enable_key_repeat ~delay:1 ~interval:1 ();
  while true do
    match Sdlevent.wait_event () with
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_ESCAPE } ->
        Sdl.quit ()
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_w } ->
        mutex_helper world_state Common.Up
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_a } ->
        mutex_helper world_state Common.Right
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_s } ->
        mutex_helper world_state Common.Down
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_d } ->
        mutex_helper world_state Common.Left
    | _ -> mutex_helper world_state Common.Nothing
  done

(*lock mutex and set action related to key pressed then unlock*)
