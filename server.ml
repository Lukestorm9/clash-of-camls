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

(** Load in some weapons, which are used all over the place. *)
let weapons = Loader.load_weapons ()

(** [find_weapon str] finds the loaded weapon associated with str. *)
let find_weapon str : Common.weapon =
  List.find (fun (s : Common.weapon) -> s.name = str) weapons

(** A weighted list of weapons to use for generating random weapons. *)
let weighted_weapons =
  [
    find_weapon "fists";
    find_weapon "sword";
    find_weapon "fists";
    find_weapon "sword";
    find_weapon "fists";
  ]

(** [weapon_change_if e weapon_name cost] possibly adds the weapon_name
    to the player inventory if the player can afford it. *)
let weapon_change_if (e : Common.entity) weapon_name cost =
  if e.points >= cost then
    {
      e with
      inventory = find_weapon weapon_name :: e.inventory;
      points = e.points - cost;
    }
  else e

(** [do_buy e i] computes the result of player e attempting to buy
    weapon i*)
let do_buy (state : Common.serv_state) (e : Common.entity) i =
  let merchant = Option.get state.data.(1) in
  if
    ((e.x -. merchant.x) ** 2.) +. ((e.y -. merchant.y) ** 2.) < 100000.
  then
    match i with
    | 1 -> weapon_change_if e "sword" 10
    | 2 -> weapon_change_if e "swordmk2" 1
    | 3 -> weapon_change_if e "hand of judgement" 100
    | _ -> e
  else e

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
      | Common.Buy i -> Some (do_buy state e i)
    in
    state.data.(idex) <- noveau
  in
  match idex with None -> () | Some i -> perform i

(** [user_send_update_loop] The connection loop for a particular user.
    Loops forever. Suffers an exception when the user disconnects, which
    kill the thread. This behavior is intentional. *)
let user_send_update_loop
    ((conn, state) : Unix.file_descr * Common.serv_state) =
  let send_chan = Unix.out_channel_of_descr conn in
  let recv_chan = Unix.in_channel_of_descr conn in
  let a = (Marshal.from_channel recv_chan : int) in
  print_endline ("logon w/ token = " ^ string_of_int a);
  Mutex.lock state.mutex;
  let angle = Random.float (2. *. 3.1415) in
  let x = 3000. *. cos angle in
  let y = 3000. *. sin angle in
  let uuid =
    insert_entity state Player x y 0. 0. "character" 100.
      [ find_weapon "fists" ] 10
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

(** [network_loop port_state_pair] listens for new inbound connections
    on [port], and spins them off onto new threads. *)
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

(** [apply_physics_step e] applies the physics step to entity e, that is
    move it according to it's velocity. *)
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

(** [respawn_player e] respawns the player e *)
let respawn_player (e : Common.entity) : Common.entity option =
  let angle = Random.float (2. *. 3.1415) in
  Some
    {
      kind = Player;
      uuid = e.uuid;
      x = 3000. *. cos angle;
      y = 3000. *. sin angle;
      vx = 0.;
      vy = 0.;
      time_sent_over = Unix.gettimeofday ();
      graphic = e.graphic;
      health = 100.;
      max_health = 100.;
      last_direction_moved = false;
      inventory = [ find_weapon "fists" ];
      points = max (e.points / 2) 0;
      last_attack_time = 0.;
    }

(** [check_dead e] checks if the entity e is dead. If so, replaces it
    with either a respawned entity in the case of a player, or None in
    the case of an entity. *)
let check_dead (e : Common.entity) =
  match e.kind with
  | Player ->
      if e.health > 0. then Some e
      else (
        print_endline
          ( "Player uuid = " ^ string_of_int e.uuid
          ^ " died with points = " ^ string_of_int e.points );
        respawn_player e )
  | _ -> if e.health > 0. then Some e else None

(** [merchant_walk e] has the merchant walk around in a circle. Simple
    enough not to require testing. Requires that e is in fact a Merchant
    or the Trailer. *)
let merchant_walk (e : Common.entity) =
  let angle =
    (Unix.gettimeofday () /. 50.) +. (float_of_int e.uuid /. 30.)
  in
  {
    e with
    x = 5000. *. cos angle;
    y = 5000. *. sin angle;
    vx = 300. *. sin angle;
    vy = 300. *. cos angle;
  }

(** [tick_state state i e] ticks the state of the world, that is, it
    applies the relevant AI step for an entity. Only do operations on
    objects which exist (i.e. are not None) *)
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
        | Merchant | Trailer -> merchant_walk e |> check_dead
        | Player | Physik -> apply_physics_step e |> check_dead )
  in
  state.data.(i) <- res

let enemies = Loader.load_enemies ()

let find_enemy str : Loader.enemy =
  List.find (fun (s : Loader.enemy) -> s.name = str) enemies

(** [entity_of_enemy enemy x y] turns the enemy prototype into an entity
    with the required state at x,y. *)
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
  let inf = find_enemy "dromedarius inferior" in
  let normal = find_enemy "dromedarius" in
  let sup = find_enemy "dromedarius superior" in
  let options = [ inf; normal; normal; sup ] in
  let max = 2 + Random.int 3 in
  for i = 1 to max do
    match find_next_open state.data with
    | Some uuid ->
        let choice = Random.int (List.length options) in
        let x = x +. Random.float 500. -. 250. in
        let y = y +. Random.float 500. -. 250. in
        let entity = entity_of_enemy (List.nth options choice) x y in
        let entity = { entity with uuid } in
        state.data.(uuid) <- Some entity
    | None -> ()
  done;
  insert_entity state (Camel None) x y 0. 0. "camel" 10. [] (-10)
  |> ignore

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
    Array.iteri (tick_state state) state.data;

    if compute_capacity state.data < 150 then
      spawn_enemy_cluster state points;

    Mutex.unlock state.mutex
  done

(** [gen_spawn_points i] generates i spawn points to use. *)
let rec gen_spawn_points i =
  if i <= 0 then []
  else
    let angle = Random.float (2. *. 3.1415) in
    let radius = 2000. +. Random.float 3000. in
    let x = radius *. cos angle in
    let y = radius *. sin angle in
    (x, y) :: gen_spawn_points (i - 1)

(** [initial_state_set state] sets the initial game state to a useable
    one, with a merchant, boss, etc. *)
let initial_state_set (state : Common.serv_state) =
  insert_entity state Trailer (-450.) 10. 0. 0. "trailer" 100. [] (-10)
  |> ignore;
  insert_entity state Merchant (-460.) 140. 0. 0. "trader" 100. [] (-10)
  |> ignore;
  insert_entity state Physik (-100.) 0. 0. 0. "golden_camel" 100. []
    (-10)
  |> ignore;
  let boss =
    entity_of_enemy (find_enemy "dromedarius ultimus") 0. (-100.)
  in
  state.data.(3) <- Some boss

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
  initial_state_set state;
  let spawn_points = gen_spawn_points 30 in
  List.iter
    (fun (x, y) ->
      insert_entity state Physik x y 0. 0. "oasis256" 100. [] (-10)
      |> ignore)
    spawn_points;

  ( Thread.create physics_loop (spawn_points, state),
    Thread.create network_loop (port, state) )
