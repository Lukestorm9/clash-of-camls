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
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_DOWN } ->
        mutex_helper world_state (Common.Attack Down)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_UP } ->
        mutex_helper world_state (Common.Attack Up)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_RIGHT } ->
        mutex_helper world_state (Common.Attack Right)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_LEFT } ->
        mutex_helper world_state (Common.Attack Left)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_w } ->
        mutex_helper world_state (Common.Move Up)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_a } ->
        mutex_helper world_state (Common.Move Right)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_s } ->
        mutex_helper world_state (Common.Move Down)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_d } ->
        mutex_helper world_state (Common.Move Left)
    | Sdlevent.KEYUP _ -> mutex_helper world_state Common.Nothing
    | _ -> ()
  done

(*lock mutex and set action related to key pressed then unlock*)
