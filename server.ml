(* Define a type for the server *)
type world_state = {
  data : Common.entity option array;
  mutex : Mutex.t;
  highest_uuid : int ref;
}

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
let insert_entity state x y vx vy graphic health =
  let uuid = !(state.highest_uuid) + 1 in
  let now = Unix.gettimeofday () in
  let entity : Common.entity =
    {
      uuid;
      x;
      y;
      vx;
      vy;
      time_sent_over = now;
      graphic;
      health;
      last_direction_moved = false;
    }
  in
  match find_next_open state.data with
  | Some i ->
      state.data.(i) <- Some entity;
      state.highest_uuid := uuid;
      Some uuid
  (* TODO: fail another way? *)
  | None ->
      print_endline "Failed to insert entity!";
      None

(* Requires the mutex to be held*)
let do_action state uuid action =
  let idex =
    Common.array_index_of
      (fun (e : Common.entity) -> e.uuid = uuid)
      state.data
  in
  let perform idex =
    let e = Option.get state.data.(idex) in
    let noveau =
      match action with
      | Common.Left ->
          Some
            {
              e with
              x = e.x +. 10.;
              vx = 10.;
              last_direction_moved = false;
            }
      | Common.Right ->
          Some
            {
              e with
              x = e.x -. 10.;
              vx = -10.;
              last_direction_moved = true;
            }
      | Common.Up -> Some { e with y = e.y -. 10.; vy = -10. }
      | Common.Down -> Some { e with y = e.y +. 10.; vy = 10. }
      | Common.Nothing -> Some { e with vx = 0.; vy = 0. }
    in
    state.data.(idex) <- noveau
  in
  match idex with None -> () | Some i -> perform i

(* The connection loop for a particular user. Loops forever. Suffers an
   exception when the user disconnects, which kill the thread. This
   behavior is intentional. *)
let user_send_update_loop (conn, state) =
  let send_chan = Unix.out_channel_of_descr conn in
  let recv_chan = Unix.in_channel_of_descr conn in
  let choice = Random.int 5 in
  let model =
    List.nth
      [ "character"; "trader"; "trailer"; "camel"; "dromedary" ]
      choice
  in
  Mutex.lock state.mutex;
  let uuid = insert_entity state 0. 0. 0. 0. model 10. in
  Mutex.unlock state.mutex;
  (* Maybe send some sort of an error message to the client? *)
  if Option.is_none uuid then ();

  try
    (* Using Marshal because we may transmit additional client logon
       information *)
    let uuid = Option.get uuid in
    Marshal.to_channel send_chan uuid [];
    flush send_chan;

    while true do
      Thread.delay 0.025;
      (* State read step *)
      Mutex.lock state.mutex;
      let copy = Array.copy state.data in
      Mutex.unlock state.mutex;

      (* State send step *)
      Marshal.to_channel send_chan copy [];
      flush send_chan;

      (* User action receive step *)
      let (uuid, action) =
        (Marshal.from_channel recv_chan : int * Common.action)
      in
      Mutex.lock state.mutex;
      do_action state uuid action;
      Mutex.unlock state.mutex
    done;
    ()
  with _ ->
    print_endline "Client Thread Error: Terminating connection";
    close_in_noerr recv_chan;
    close_out_noerr send_chan

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
let apply_physics_step time (state : world_state) i e : Common.entity =
  let now = Unix.gettimeofday () in
  let delta = ((now -. time) /. 3.) +. (3.14 /. 6. *. float_of_int i) in
  {
    e with
    x = 300. *. cos delta;
    y = 300. *. sin delta;
    vx = -100. *. sin delta;
    vy = 100. *. cos delta;
    last_direction_moved = -300. *. sin delta <= 0.;
  }

(* Only do physics operations on objects which exist *)
let filter_objects time state i = function
  | None -> None
  | Some e ->
      if i <= 4 then Some (apply_physics_step time state i e)
      else Some e

(* The physics loop, which is running all the time in the background.
   Set to run at approx 20 ticks per second. TODO: make time exact. *)
let physics_loop state =
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
  let state : world_state =
    {
      data = Array.make 500 None;
      mutex = Mutex.create ();
      highest_uuid = ref 0;
    }
  in
  (* TODO: remove these -- In MS2, these will be replaced by random
     generation algorithm *)
  insert_entity state 0. 0. 0. 0. "dromedary" 10. |> ignore;
  insert_entity state 0. 0. 0. 0. "trailer" 10. |> ignore;
  insert_entity state 0. 0. 0. 0. "trader" 10. |> ignore;
  insert_entity state 0. 0. 0. 0. "camel" 10. |> ignore;
  insert_entity state 0. 0. 0. 0. "character" 10. |> ignore;

  ( Thread.create physics_loop state,
    Thread.create network_loop (port, state) )
