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
  let last_action_was_buy = ref false in
  while true do
    match Sdlevent.wait_event () with
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_ESCAPE } ->
        Sdl.quit ()
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_DOWN } ->
        mutex_helper world_state (Common.Attack Up);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_UP } ->
        mutex_helper world_state (Common.Attack Down);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_RIGHT } ->
        mutex_helper world_state (Common.Attack Right);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_LEFT } ->
        mutex_helper world_state (Common.Attack Left);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_w } ->
        mutex_helper world_state (Common.Move Up);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_a } ->
        mutex_helper world_state (Common.Move Right);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_s } ->
        mutex_helper world_state (Common.Move Down);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_d } ->
        mutex_helper world_state (Common.Move Left);
        last_action_was_buy := false
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_1 } ->
        if not !last_action_was_buy then
          mutex_helper world_state (Common.Buy 1);
        last_action_was_buy := true
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_2 } ->
        if not !last_action_was_buy then
          mutex_helper world_state (Common.Buy 2);
        last_action_was_buy := true
    | Sdlevent.KEYDOWN { Sdlevent.keysym = Sdlkey.KEY_3 } ->
        if not !last_action_was_buy then
          mutex_helper world_state (Common.Buy 3);
        last_action_was_buy := true
    | Sdlevent.KEYUP _ -> mutex_helper world_state Common.Nothing
    | _ -> ()
  done
