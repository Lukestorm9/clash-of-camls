let radius = 250000.

(** [distance] finds the distance between two points on x-y coordinate
    plane WITHOUT taking the square root*)
let distance (x1 : float) (y1 : float) (x2 : float) (y2 : float) : float
    =
  ((x2 -. x1) ** 2.) +. ((y2 -. y1) ** 2.)

(** [inside cirlce] determines if a pair of points on the x,y plane are
    within a given radius (r)*)
let inside_circle x1 y1 x2 y2 (r : float) : bool =
  let d = distance x1 y1 x2 y2 in
  d <= r ** 2.

(** [modify_entity] modifies a given entity by moving its x,y position
    given vx, vy, time_sent_over and current system time. None of the
    other parameeters of entity are changed.*)
let modify_entity (entity : Common.entity) =
  {
    entity with
    x =
      entity.x
      +. (entity.vx *. (Unix.gettimeofday () -. entity.time_sent_over));
    y =
      entity.y
      +. (entity.vy *. (Unix.gettimeofday () -. entity.time_sent_over));
  }

(** [location_smoothing] takes a world and modifies x,y location of each
    entity to match where that entity will be in future based on
    time_sent_over, vx, and vy*)
let rec location_smoothing
    (world : Common.entity list)
    (acc : Common.entity list) : Common.entity list =
  match world with
  | [] -> acc
  | h :: t ->
      let modified_entity = modify_entity h in
      location_smoothing t (modified_entity :: acc)

(** [get_local] calculates "local" entities and applies
    location_smoothing and applies a Mutex*)
let get_local (state : Common.world_state) (x : float) (y : float) =
  let inside_circle (t : Common.entity) =
    inside_circle t.x t.y x y radius
  in
  Mutex.lock state.mutex;
  let local_object = Common.array_filter inside_circle state.data in
  Mutex.unlock state.mutex;
  location_smoothing local_object []

(** [xy] gets a player's xy position*)
let xy (state : Common.world_state) =
  let uuid = state.uuid in
  match !uuid with
  | Some uuid -> (
      let candidates =
        Common.array_filter
          (fun (entity : Common.entity) -> entity.uuid = uuid)
          state.data
      in
      match candidates with
      | [] -> None
      | h :: _ ->
          let corrected = modify_entity h in
          Some (corrected.x, corrected.y) )
  | None -> None

(* [get_player_xy state] gets players's x,y position and applies a Mutex*)
let get_player_xy (state : Common.world_state) =
  Mutex.lock state.mutex;
  let location = xy state in
  Mutex.unlock state.mutex;
  location

(** [get_player_uuid state] gets the player uuid*)
let get_player_uuid (state : Common.world_state) =
  Mutex.lock state.mutex;
  let uuid = state.uuid.contents in
  Mutex.unlock state.mutex;
  uuid

(** [get_winner_opt state] returns the winner of the game, if any.*)
let get_winner_opt (state : Common.world_state) =
  Mutex.lock state.mutex;
  let winner = state.winner.contents in
  Mutex.unlock state.mutex;
  winner
