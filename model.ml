(* [norm a b] is just the standard norm of a 2-vector *)
let norm a b = sqrt ((a *. a) +. (b *. b))

(* [norm_entity_velocity e] normalizes the entity e's velocity to
   reasonable bounds. *)
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
let get_local_enemies
    (state : Common.serv_state)
    (entity : Common.entity)
    radius
    direction =
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

let process_attack
    (state : Common.serv_state)
    (entity : Common.entity)
    direction =
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

let closest (state : Common.serv_state) (e : Common.entity) range kind =
  let closest = ref (range, None) in
  Array.iteri
    (fun i -> function
      | Some entity ->
          closest := update_min_dist !closest entity i e kind
      | None -> ())
    state.data;
  !closest

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

(* entity must be a camel *)
let try_acquire_imprint state (e : Common.entity) =
  match closest state e 250000. Player with
  | _, Some (i, closest) -> { e with kind = Camel (Some i) }
  | _, None -> e

(* Compute AI movement for a single object at a single time. *)
let apply_enemy_step (state : Common.serv_state) (e : Common.entity) :
    Common.entity =
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
