(* Find the next open slot in the array *)
let find_next_open array =
  let filter (acc, i) v =
    match acc with
    | None -> if Option.is_none v then (Some i, i + 1) else (None, i + 1)
    | Some j -> (Some j, i + 1)
  in
  Array.fold_left filter (None, 0) array |> fst

(* Construct a new entity with the desired qualities, then insert it
   into the world state. Requires the state mutex to be locked on the
   current thread BEFORE this method is called.*)
let insert_entity (state : Common.world_state) x y vx vy graphic health
    =
  let uuid = !(state.highest_uuid) + 1 in
  let now = Unix.gettimeofday () in
  let entity : Common.entity =
    { uuid; x; y; vx; vy; time_sent_over = now; graphic; health }
  in
  match find_next_open state.data with
  | Some i -> state.data.(i) <- Some entity
  (* TODO: fail another way? *)
  | None -> print_endline "Failed to insert entity!"

(* The connection loop for a particular user. Loops forever. Suffers an
   exception when the user disconnects, which kill the thread. This
   behavior is intentional. *)
let user_send_update_loop
    ((conn, state) : Unix.file_descr * Common.world_state) =
  let send_chan = Unix.out_channel_of_descr conn in
  while true do
    Thread.delay 0.05;
    Mutex.lock state.mutex;
    Marshal.to_channel send_chan state.data [];
    flush send_chan;
    (* In the future, send over highest_uuid here *)
    Option.get state.data.(0) |> fun e ->
    e.x |> string_of_float |> print_endline;
    Mutex.unlock state.mutex
  done;
  ()

(* Listen for new inbound connections, and spin them off onto new
   threads. *)
let network_loop (port, state) =
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.setsockopt sock SO_REUSEADDR true;
  Unix.bind sock (ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", port));
  Unix.listen sock 10;
  while true do
    let conn = Unix.accept sock |> fst in
    Thread.create user_send_update_loop (conn, state) |> ignore
  done;
  ()

(* Compute physics for a single object at a single time. This is an
   immutable operation, and a new object is returned. TODO: This will be
   replaced with something proper in a future version. *)
let apply_physics_step time (state : Common.world_state) i e :
    Common.entity =
  let now = Unix.gettimeofday () in
  let delta = now -. time +. (3. *. float_of_int i) in
  {
    e with
    x = 300. +. (300. *. cos delta);
    y = 300. +. (300. *. sin delta);
    vx = -500. *. sin delta;
    vy = 500. *. cos delta;
    time_sent_over = now;
  }

(* Only do physics operations on objects which exist *)
let filter_objects time (state : Common.world_state) i = function
  | None -> None
  | Some e -> Some (apply_physics_step time state i e)

(* The physics loop, which is running all the time in the background.
   Set to run at approx 20 ticks per second. TODO: make time exact. *)
let physics_loop (state : Common.world_state) =
  let start = Unix.gettimeofday () in
  while true do
    Thread.delay 0.05;
    Mutex.lock state.mutex;
    let copy = Array.copy state.data in
    Mutex.unlock state.mutex;

    let noveau = Array.mapi (filter_objects start state) copy in

    Mutex.lock state.mutex;
    let len = Array.length noveau in
    Array.blit noveau 0 state.data 0 len;
    Mutex.unlock state.mutex
  done;
  ()

(* Start the physics and networking threads *)
let start port =
  Sys.set_signal Sys.sigpipe Signal_ignore;
  let state : Common.world_state =
    {
      data = Array.make 500 None;
      mutex = Mutex.create ();
      highest_uuid = ref 0;
    }
  in
  insert_entity state 0. 0. 0. 0. "character.png" 10.;
  insert_entity state 0. 0. 0. 0. "character.png" 10.;

  ( Thread.create physics_loop state,
    Thread.create network_loop (port, state) )
