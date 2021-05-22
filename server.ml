(** [find_next_open array] finds the uuid corresponding to the next open
    (None) slot in the state array, and then returns that uuid. While
    superficially similar to List find this function by necessity
    returns an index for the purpose of state manipulation.. Note that
    OCAML's arrays and lists do not provide equivalent functionality by
    default: https://ocaml.org/api/Array.html
    https://ocaml.org/api/List.html . *)
let find_next_open array =
  let filter (acc, i) v =
    match acc with
    | None -> if Option.is_none v then (Some i, i + 1) else (None, i + 1)
    | Some j -> (Some j, i + 1)
  in
  Array.fold_left filter (None, 0) array |> fst

(** [insert_entity] constructs a new entity with the desired qualities,
    then insert it into the world state. Requires the state mutex to be
    locked on the current thread BEFORE this method is called.*)
let insert_entity
    (state : Common.serv_state)
    kind
    x
    y
    vx
    vy
    graphic
    health
    inv
    points =
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
  | None ->
      print_endline "Failed to insert entity!";
      None

(** [do_action state uuid actions] has the player with uuid do the
    action specified. Requires the state mutex to be held *)
let do_action (state : Common.serv_state) uuid action =
  let idex =
    Common.array_index_of
      (fun (e : Common.entity) -> e.uuid = uuid)
      state.data
  in
  let perform idex =
    let e = Option.get state.data.(idex) in
    let noveau =
      match action with
      | Common.Move d -> Some (Model.process_movement e d)
      | Common.Attack d -> Some (Model.process_attack state e d)
      | Common.Nothing -> Some { e with vx = 0.; vy = 0. }
      | Common.Buy _ -> Some e
    in
    state.data.(idex) <- noveau
  in
  match idex with None -> () | Some i -> perform i

(** Load in some weapons, which are used all over the place. *)
let weapons = Loader.load_weapons ()

let find_weapon str : Common.weapon =
  List.find (fun (s : Common.weapon) -> s.name = str) weapons

(* The connection loop for a particular user. Loops forever. Suffers an
   exception when the user disconnects, which kill the thread. This
   behavior is intentional. *)
let user_send_update_loop
    ((conn, state) : Unix.file_descr * Common.serv_state) =
  let send_chan = Unix.out_channel_of_descr conn in
  let recv_chan = Unix.in_channel_of_descr conn in
  let a = (Marshal.from_channel recv_chan : int) in
  print_endline ("logon w/ token = " ^ string_of_int a);
  Mutex.lock state.mutex;
  let angle = Random.float (2. *. 3.1415) in
  let x = 300. *. cos angle in
  let y = 300. *. sin angle in
  let weapons =
    [
      find_weapon "fists";
      find_weapon "sword";
      find_weapon "fists";
      find_weapon "sword";
      find_weapon "fists";
    ]
  in
  let weapon_idx = Random.int (List.length weapons) in
  let uuid =
    insert_entity state Player x y 0. 0. "character" 100.
      [
        List.nth weapons weapon_idx;
        find_weapon "sword";
        find_weapon "fists";
      ]
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

let respawn_player weapons (e : Common.entity) : Common.entity option =
  let angle = Random.float (2. *. 3.1415) in
  let weapon_idx = Random.int (List.length weapons) in
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
      inventory = [ List.nth weapons weapon_idx ];
      points = max (e.points / 2) 0;
      last_attack_time = 0.;
    }

let check_dead (e : Common.entity) =
  match e.kind with
  | Player ->
      if e.health > 0. then Some e
      else (
        print_endline
          ( "Player uuid = " ^ string_of_int e.uuid
          ^ " died with points = " ^ string_of_int e.points );
        let weapons =
          [
            find_weapon "fists";
            find_weapon "sword";
            find_weapon "fists";
            find_weapon "sword";
            find_weapon "fists";
          ]
        in
        respawn_player weapons e )
  | _ -> if e.health > 0. then Some e else None

(* Only do physics operations on objects which exist *)
let tick_state (state : Common.serv_state) i (e : Common.entity option)
    =
  let res =
    match e with
    | None -> None
    | Some e -> (
        match e.kind with
        | Ai ->
            Model.apply_enemy_step state e
            |> apply_physics_step |> check_dead
        | Camel None ->
            Model.try_acquire_imprint state e
            |> apply_physics_step |> check_dead
        | Camel (Some i) ->
            Model.follow state e |> apply_physics_step |> check_dead
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
    inventory = [ enemy.weapon ];
    last_attack_time = 0.;
    time_sent_over = Unix.gettimeofday ();
    points = enemy.points;
  }

(** [spawn_enemy_cluster state points] spawns an enemy cluster *)
let spawn_enemy_cluster (state : Common.serv_state) points =
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

(** [physics_loop point_state_pair] is the physics loop, which is
    running all the time in the background. Set to run at approx 20
    ticks per second. TODO: make time exact. *)
let physics_loop
    ((points, state) : (float * float) list * Common.serv_state) =
  let compute_capacity array =
    Array.fold_left
      (fun acc v -> if Option.is_some v then acc + 1 else acc)
      0 array
  in

  while true do
    Thread.delay 0.05;

    Mutex.lock state.mutex;

    (* Do AI/Physics calculations*)
    Array.iteri (tick_state state) state.data;

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
    let radius = 2000. +. Random.float 4000. in
    let x = radius *. cos angle in
    let y = radius *. sin angle in
    (x, y) :: gen_spawn_points (i - 1)

(** [start port] starts the server with the specified port. In
    particular, it starts the physics and networking threads, sets up
    the map, and handles similar configuration*)
let start port =
  Sys.set_signal Sys.sigpipe Signal_ignore;
  let state : Common.serv_state =
    {
      data = Array.make 500 None;
      mutex = Mutex.create ();
      points_gathered = ref 0;
    }
  in
  insert_entity state (Camel (Some 1)) (-450.) 10. 0. 0. "trailer" 100.
    [] (-10)
  |> ignore;
  insert_entity state Merchant (-460.) 140. 0. 0. "trader" 100. [] (-10)
  |> ignore;
  insert_entity state Physik (-100.) 0. 0. 0. "golden_camel" 100. []
    (-10)
  |> ignore;
  let spawn_points = gen_spawn_points 30 in
  List.iter
    (fun (x, y) ->
      insert_entity state Physik x y 0. 0. "oasis256" 100. [] (-10)
      |> ignore)
    spawn_points;

  ( Thread.create physics_loop (spawn_points, state),
    Thread.create network_loop (port, state) )
