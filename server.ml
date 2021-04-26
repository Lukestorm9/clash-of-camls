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
let insert_entity state kind x y vx vy graphic health inv =
  let uuid = !(state.highest_uuid) + 1 in
  let now = Unix.gettimeofday () in
  let entity : Common.entity =
    {
      kind;
      uuid;
      x;
      y;
      vx;
      vy;
      time_sent_over = now;
      graphic;
      health;
      max_health = health;
      last_direction_moved = false;
      inventory = inv;
      points = 0;
      last_attack_time = 0.;
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

let process_movement (e : Common.entity) = function
  | Common.Left -> { e with vx = 200.; last_direction_moved = false }
  | Common.Right -> { e with vx = -200.; last_direction_moved = true }
  | Common.Up -> { e with vy = -200. }
  | Common.Down -> { e with vy = 200. }

let norm a b = sqrt ((a *. a) +. (b *. b))

let inside_directed_circle
    e_x
    e_y
    x
    y
    radius
    (direction : Common.direction) =
  let dx = x -. e_x in
  let dy = y -. e_y in
  let distance = sqrt ((dx *. dx) +. (dy *. dy)) in
  let angle =
    if dy < 0. then acos (dx /. distance) *. -1.
    else acos (dx /. distance)
  in
  print_endline (string_of_float x ^ " " ^ string_of_float y);
  print_endline (string_of_float angle);
  let pi = 3.14159 in
  let pi_4 = pi /. 4. in
  let pi_4' = pi_4 *. -1. in
  let pi_3_4 = 3. *. pi /. 4. in
  let pi_3_4' = pi_3_4 *. -1. in
  if distance <= radius then
    match direction with
    | Up -> angle <= pi_3_4 && angle >= pi_4
    | Down -> angle >= pi_3_4' && angle <= pi_4'
    | Right -> angle >= pi_3_4 || angle <= pi_3_4'
    | Left -> angle <= pi_4 && angle <= pi_4'
  else false

(**[get_local_enemies] Given word_state entity and radius return the
   entity list of enemies that are possible are attack*)
let get_local_enemies state (entity : Common.entity) radius direction =
  let index_data =
    Array.mapi (fun i e -> Option.map (fun e -> (i, e)) e) state.data
  in
  if radius < 0. then []
  else
    Common.array_filter
      (fun ((i, e) : int * Common.entity) ->
        e.uuid <> entity.uuid
        && inside_directed_circle entity.x entity.y e.x e.y radius
             direction)
      index_data

let process_attack state (entity : Common.entity) direction =
  let weapon = List.hd entity.inventory in
  let now = Unix.gettimeofday () in
  if now -. entity.last_attack_time > weapon.cooldown then (
    let enemies =
      get_local_enemies state entity weapon.range direction
    in
    List.iter
      (fun ((i, e) : int * Common.entity) ->
        state.data.(i) <-
          Some { e with health = e.health -. weapon.damage })
      enemies;
    { entity with last_attack_time = now } )
  else entity

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
      | Common.Move d -> Some (process_movement e d)
      | Common.Attack d -> Some (process_attack state e d)
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
  let a = (Marshal.from_channel recv_chan : int) in
  print_endline ("logon w/ token = " ^ string_of_int a);
  Mutex.lock state.mutex;
  let uuid =
    insert_entity state Player 0. 0. 0. 0. "character" 100.
      [
        { name = "fists"; range = 250.; damage = 20.; cooldown = 0.25 };
      ]
  in
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

(* Compare the distance information encoded in last with e1 <-> e2 if e1
   is a player, picking whichever is least. *)
let update_min_dist last (e1 : Common.entity) i (e2 : Common.entity) =
  match e1.kind with
  | Player ->
      let dx = e1.x -. e2.x in
      let dy = e1.y -. e2.y in
      let dst2 = (dx *. dx) +. (dy *. dy) in
      if dst2 > 2500. && dst2 < (last |> fst) then (dst2, Some (i, e1))
      else last
  | _ -> last

(* Compute AI movement for a single object at a single time. This is an
   immutable operation, and a new object is returned. TODO: This will be
   replaced with something proper in a future version. *)
let apply_ai_step state i e : Common.entity =
  let closest = ref (250000., None) in
  Array.iteri
    (fun i -> function
      | Some entity -> closest := update_min_dist !closest entity i e
      | None -> ())
    state;
  match !closest with
  | dst2, Some (i, closest) ->
      let dx = closest.x -. e.x in
      let dy = closest.y -. e.y in
      let norm = 1. +. norm dx dy in
      let dvx = 100. *. dx /. norm in
      let dvy = 100. *. dy /. norm in
      ( match e.inventory with
      | h :: _ -> if dst2 < h.range ** 2. && true then ()
      | _ -> () );
      { e with vx = dvx; vy = dvy }
  | _, None -> { e with vx = 0.; vy = 0. }

let apply_physics_step i (e : Common.entity) : Common.entity =
  let now = Unix.gettimeofday () in
  {
    e with
    x = e.x +. (e.vx *. (now -. e.time_sent_over));
    y = e.y +. (e.vy *. (now -. e.time_sent_over));
    last_direction_moved = e.vx < 0.;
    time_sent_over = now;
  }

let check_dead (e : Common.entity) =
  if e.health > 0. then Some e else None

(* Only do physics operations on objects which exist *)
let filter_objects state i (e : Common.entity option) =
  match e with
  | None -> None
  | Some e -> (
      match e.kind with
      | Ai ->
          apply_ai_step state i e |> apply_physics_step i |> check_dead
      | Player | Physik -> apply_physics_step i e |> check_dead )

(* The physics loop, which is running all the time in the background.
   Set to run at approx 20 ticks per second. TODO: make time exact. *)
let physics_loop state =
  while true do
    Thread.delay 0.05;

    (* Do AI/Physics calculations*)
    Mutex.lock state.mutex;
    let noveau = Array.mapi (filter_objects state.data) state.data in
    let len = Array.length noveau in
    Array.blit noveau 0 state.data 0 len;
    Mutex.unlock state.mutex
  done;
  ()

(* Start the physics and networking threads *)
let start port =
  let enemies = Loader.load_enemies () in
  let weapons = Loader.load_weapons () in

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
  insert_entity state Ai 500. 0. 0. 0. "dromedary" 50. []
  |> ignore |> ignore |> ignore;
  insert_entity state Ai (-500.) 0. 0. 0. "trailer" 50. []
  |> ignore |> ignore |> ignore;
  insert_entity state Ai 0. 500. 0. 0. "trader" 50. []
  |> ignore |> ignore |> ignore;
  insert_entity state Ai 0. (-500.) 0. 0. "camel" 50. []
  |> ignore |> ignore |> ignore;

  ( Thread.create physics_loop state,
    Thread.create network_loop (port, state) )
