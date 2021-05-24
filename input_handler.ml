(** [mutex_helper world_state action] applies [action] to [world_state] *)
let mutex_helper
    (world_state : Common.world_state)
    (action : Common.action) =
  Mutex.lock world_state.mutex;
  world_state.user_command := action;
  Mutex.unlock world_state.mutex

(** [key_checker world] checks if a key is pressed down and then applies
    a specific action based on the key pressed to [world]. *)
let key_checker (world_state : Common.world_state) =
  Sdlkey.enable_key_repeat ~delay:1 ~interval:1 ();
  while true do
    match Sdlevent.wait_event () with
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_ESCAPE } ->
        Sdl.quit ()
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_DOWN } ->
        mutex_helper world_state (Common.Attack Up)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_UP } ->
        mutex_helper world_state (Common.Attack Down)
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
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_1 } ->
        mutex_helper world_state (Common.Buy 1)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_2 } ->
        mutex_helper world_state (Common.Buy 2)
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_3 } ->
        mutex_helper world_state (Common.Buy 3)
    | Sdlevent.KEYUP _ -> mutex_helper world_state Common.Nothing
    | _ -> ()
  done
