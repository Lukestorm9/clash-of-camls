(* Define a type for the server *)
type world_state = {
  data : Common.entity option array;
  points_gathered : int ref;
  mutex : Mutex.t;
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
let insert_entity state kind x y vx vy graphic health inv points =
  let now = Unix.gettimeofday () in
  let entity : Common.entity =
    {
      kind;
      uuid = -1;
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
      points;
      last_attack_time = 0.;
    }
  in
  match find_next_open state.data with
  | Some i ->
      state.data.(i) <- Some { entity with uuid = i };
      Some i
  (* TODO: fail another way? *)
  | None ->
      print_endline "Failed to insert entity!";
      None

let norm a b = sqrt ((a *. a) +. (b *. b))

let norm_entity_velocity (e : Common.entity) =
  let nvx =
    if e.vx > 0. then 300. else if e.vx < 0. then -300. else 0.
  in
  let nvy =
    if e.vy > 0. then 300. else if e.vy < 0. then -300. else 0.
  in
  let norm = norm nvx nvy /. 300. in

  { e with vx = nvx /. norm; vy = nvy /. norm }

let process_movement (e : Common.entity) = function
  | Common.Left ->
      { e with vx = 300.; last_direction_moved = false }
      |> norm_entity_velocity
  | Common.Right ->
      { e with vx = -300.; last_direction_moved = true }
      |> norm_entity_velocity
  | Common.Up -> { e with vy = -300. } |> norm_entity_velocity
  | Common.Down -> { e with vy = 300. } |> norm_entity_velocity

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
  let pi = 3.14159 in
  let pi_4 = pi /. 4. in
  let pi_4' = pi_4 *. -1. in
  let pi_3_4 = 3. *. pi /. 4. in
  let pi_3_4' = pi_3_4 *. -1. in
  if distance <= radius then
    match direction with
    | Up -> angle <= pi_3_4 && angle >= pi_4
    | Down -> angle >= pi_3_4' && angle <= pi_4'
    | Right -> angle <= pi_4 && angle >= pi_4'
    | Left -> angle >= pi_3_4 || angle <= pi_3_4'
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
        && (e.kind = Player || e.kind = Ai)
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
    let extra_pts = ref 0 in
    List.iter
      (fun ((i, e) : int * Common.entity) ->
        let e = { e with health = e.health -. weapon.damage } in
        state.data.(i) <- Some e;
        if e.health <= 0. then (
          extra_pts := !extra_pts + e.points;
          state.points_gathered :=
            e.points + state.points_gathered.contents ))
      enemies;
    {
      entity with
      last_attack_time = now;
      points = entity.points + !extra_pts;
    } )
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

let weapons = Loader.load_weapons ()

let into_weapon (w : Loader.weapon) : Common.weapon =
  {
    name = w.name;
    range = w.range;
    damage = w.damage;
    cooldown = w.cooldown;
  }

let find_weapon str : Common.weapon =
  List.find (fun (s : Loader.weapon) -> s.name = str) weapons
  |> into_weapon

(* The connection loop for a particular user. Loops forever. Suffers an
   exception when the user disconnects, which kill the thread. This
   behavior is intentional. *)
let user_send_update_loop (conn, state) =
  let send_chan = Unix.out_channel_of_descr conn in
  let recv_chan = Unix.in_channel_of_descr conn in
  let a = (Marshal.from_channel recv_chan : int) in
  print_endline ("logon w/ token = " ^ string_of_int a);
  Mutex.lock state.mutex;
  let angle = Random.float (2. *. 3.1415) in
  let x = 100. *. cos angle in
  let y = 100. *. sin angle in
  let weapons =
    [ find_weapon "fists"; find_weapon "fists"; find_weapon "sword" ]
  in
  let weapon_idx = Random.int (List.length weapons) in
  let uuid =
    insert_entity state Player x y 0. 0. "character" 100.
      [ find_weapon "fists"; find_weapon "sword"; find_weapon "fists" ]
      10
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
      let now = Unix.gettimeofday () in
      Marshal.to_channel send_chan (now, copy) [];
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
let update_min_dist
    last
    (e1 : Common.entity)
    i
    (e2 : Common.entity)
    kind =
  match e1.kind = kind && e1 <> e2 with
  | true ->
      let dx = e1.x -. e2.x in
      let dy = e1.y -. e2.y in
      let dst2 = (dx *. dx) +. (dy *. dy) in
      if dst2 < (last |> fst) then (dst2, Some (i, e1)) else last
  | _ -> last

let closest state (e : Common.entity) range kind =
  let closest = ref (range, None) in
  Array.iteri
    (fun i -> function
      | Some entity ->
          closest := update_min_dist !closest entity i e kind
      | None -> ())
    state.data;
  !closest

(* Compute AI movement for a single object at a single time. *)
let apply_enemy_step state i (e : Common.entity) : Common.entity =
  let now = Unix.gettimeofday () in
  (* TODO: better scaling*)
  let difficulty_value =
    2.718 ** (float_of_int state.points_gathered.contents *. 0.0025)
  in
  let difficulty_factor = 2.5 -. (4. /. (1. +. difficulty_value)) in
  match closest state e 250000. Player with
  | dst2, Some (i, closest) ->
      let dx = closest.x -. e.x in
      let dy = closest.y -. e.y in
      let norm = 1. +. norm dx dy in
      let dvx =
        if norm > 40. then difficulty_factor *. 100. *. dx /. norm
        else 0.
      in
      let dvy =
        if norm > 40. then difficulty_factor *. 100. *. dy /. norm
        else 0.
      in
      let weapon = List.hd e.inventory in
      if
        dst2 < weapon.range ** 2.
        && now -. e.last_attack_time > weapon.cooldown
      then (
        print_endline
          ( "Attacking player w/ uuid = " ^ string_of_int i ^ " from "
          ^ string_of_int e.uuid ^ " @ " ^ string_of_float e.x ^ ", "
          ^ string_of_float e.y );
        state.data.(i) <-
          Some { closest with health = closest.health -. weapon.damage };
        { e with vx = dvx; vy = dvy; last_attack_time = now } )
      else { e with vx = dvx; vy = dvy }
  | _, None -> { e with vx = 0.; vy = 0. }

(* entity must be a camel *)
let try_acquire_imprint state (e : Common.entity) =
  match closest state e 250000. Player with
  | _, Some (i, closest) -> { e with kind = Camel (Some i) }
  | _, None -> e

let follow state (e : Common.entity) =
  match e.kind with
  | Camel (Some uuid) ->
      let fx, fy =
        match closest state e 2000. (Camel (Some uuid)) with
        | dst2, Some (i, closest) ->
            let dcx = e.x -. closest.x in
            let dcy = e.y -. closest.y in
            let norm = 0.01 +. norm dcx dcy in
            (dcx *. 50. /. norm, dcy *. 50. /. norm)
        | _, None -> (0., 0.)
      in
      let target = Option.get state.data.(uuid) in
      let dx = target.x -. e.x in
      let dy = target.y -. e.y in
      let norm = 1. +. norm dx dy in
      let dvx = if norm > 150. then 250. *. dx /. norm else 0. in
      let dvy = if norm > 150. then 250. *. dy /. norm else 0. in
      { e with vx = dvx +. fx; vy = dvy +. fy }
  | _ -> e

let apply_physics_step (e : Common.entity) : Common.entity =
  let now = Unix.gettimeofday () in
  {
    e with
    x = e.x +. (e.vx *. (now -. e.time_sent_over));
    y = e.y +. (e.vy *. (now -. e.time_sent_over));
    last_direction_moved =
      (if e.vx <> 0. then e.vx < 0. else e.last_direction_moved);
    time_sent_over = now;
  }

let check_dead (e : Common.entity) =
  match e.kind with
  | Player ->
      if e.health > 0. then Some e
      else (
        print_endline
          ( "Player uuid = " ^ string_of_int e.uuid
          ^ " died with points = " ^ string_of_int e.points );
        let angle = Random.float (2. *. 3.1415) in
        Some
          {
            kind = Player;
            uuid = e.uuid;
            x = 100. *. cos angle;
            y = 100. *. sin angle;
            vx = 0.;
            vy = 0.;
            time_sent_over = Unix.gettimeofday ();
            graphic = e.graphic;
            health = 100.;
            max_health = 100.;
            last_direction_moved = false;
            inventory = e.inventory;
            points = max (e.points / 2) 0;
            last_attack_time = 0.;
          } )
  | _ -> if e.health > 0. then Some e else None

(* Only do physics operations on objects which exist *)
let filter_objects state i (e : Common.entity option) =
  let res =
    match e with
    | None -> None
    | Some e -> (
        match e.kind with
        | Ai ->
            apply_enemy_step state i e
            |> apply_physics_step |> check_dead
        | Camel None ->
            try_acquire_imprint state e
            |> apply_physics_step |> check_dead
        | Camel (Some i) ->
            follow state e |> apply_physics_step |> check_dead
        | Player | Physik | Merchant ->
            apply_physics_step e |> check_dead )
  in
  state.data.(i) <- res

let enemies = Loader.load_enemies ()

let find_enemy str : Loader.enemy =
  List.find (fun (s : Loader.enemy) -> s.name = str) enemies

let entity_of_enemy (enemy : Loader.enemy) x y : Common.entity =
  {
    kind = Ai;
    uuid = -1;
    x;
    y;
    vx = 0.;
    vy = 0.;
    health = enemy.health;
    max_health = enemy.health;
    graphic = enemy.graphic;
    last_direction_moved = Random.bool ();
    inventory = [ enemy.weapon |> into_weapon ];
    last_attack_time = 0.;
    time_sent_over = Unix.gettimeofday ();
    points = enemy.points;
  }

let spawn_enemy_cluster state points =
  let choice = Random.int (List.length points) in
  let x, y = List.nth points choice in
  let dromedarius_inf = find_enemy "dromedarius inferior" in
  let dromedarius = find_enemy "dromedarius" in
  let dromedarius_sup = find_enemy "dromedarius superior" in
  let options =
    [ dromedarius_inf; dromedarius; dromedarius; dromedarius_sup ]
  in
  let max = 2 + Random.int 3 in
  for i = 1 to max do
    match find_next_open state.data with
    | Some uuid ->
        let choice = Random.int (List.length options) in
        let x = x +. Random.float 500. -. 250. in
        let y = y +. Random.float 500. -. 250. in
        let entity = entity_of_enemy (List.nth options choice) x y in
        let entity = { entity with uuid } in
        print_endline
          ( "Creating enemy w/ uuid = " ^ string_of_int uuid ^ " at "
          ^ string_of_float x ^ ", " ^ string_of_float y );
        state.data.(uuid) <- Some entity
    | None -> ()
  done;
  insert_entity state (Camel None) x y 0. 0. "camel" 10. [] (-10)
  |> ignore;
  ()

(* The physics loop, which is running all the time in the background.
   Set to run at approx 20 ticks per second. TODO: make time exact. *)
let physics_loop (points, state) =
  let compute_capacity array =
    Array.fold_left
      (fun acc v -> if Option.is_some v then acc + 1 else acc)
      0 array
  in

  while true do
    Thread.delay 0.05;

    Mutex.lock state.mutex;

    (* Do AI/Physics calculations*)
    Array.iteri (filter_objects state) state.data;

    (* Spawn enemies if needed*)
    if compute_capacity state.data < 150 then
      spawn_enemy_cluster state points;

    Mutex.unlock state.mutex
  done;
  ()

let rec gen_spawn_points i =
  if i <= 0 then []
  else
    let angle = Random.float (2. *. 3.1415) in
    let radius = 3000. +. Random.float 6000. in
    let x = radius *. cos angle in
    let y = radius *. sin angle in
    (x, y) :: gen_spawn_points (i - 1)

(* Start the physics and networking threads *)
let start port =
  Sys.set_signal Sys.sigpipe Signal_ignore;
  let state : world_state =
    {
      data = Array.make 500 None;
      mutex = Mutex.create ();
      points_gathered = ref 0;
    }
  in
  insert_entity state Physik (-50.) 65. 0. 0. "trader" 100. [] (-10)
  |> ignore;
  insert_entity state Physik 50. (-10.) 0. 0. "trailer" 100. [] (-10)
  |> ignore;
  insert_entity state Physik 35. 25. 0. 0. "golden_camel" 100. [] (-10)
  |> ignore;
  let spawn_points = gen_spawn_points 30 in
  List.iter
    (fun (x, y) ->
      insert_entity state Physik x y 0. 0. "oasis256" 100. [] (-10)
      |> ignore)
    spawn_points;

  ( Thread.create physics_loop (spawn_points, state),
    Thread.create network_loop (port, state) )
