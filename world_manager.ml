let radius = 250000.

(**[distance] finds the distance between two points on x-y coordinate
   plane WITHOUT taking the square root*)
let distance (x1 : float) (y1 : float) (x2 : float) (y2 : float) : float
    =
  ((x2 -. x1) ** 2.) +. ((y2 -. y1) ** 2.)

(**[inside cirlce] determines if a pair of points on the x,y plane are
   within a given radius (r)*)
let inside_circle x1 y1 x2 y2 (r : float) : bool =
  let d = distance x1 y1 x2 y2 in
  d <= r ** 2.

(*[modify_h] modifies a given entity by moving its x,y position given
  vx, vy, time_sent_over and current system time. None of the other
  parameeters of entity are changed.*)
let modify_h (h : Common.entity) =
  {
    Common.uuid = h.uuid;
    x = h.x +. (h.vx *. (Unix.gettimeofday () -. h.time_sent_over));
    y = h.y +. (h.vy *. (Unix.gettimeofday () -. h.time_sent_over));
    vx = h.vx;
    vy = h.vy;
    time_sent_over = h.time_sent_over;
    graphic = h.graphic;
    health = h.health;
  }

let rec location_smoothing
    (world : Common.entity list)
    (acc : Common.entity list) : Common.entity list =
  match world with
  | [] -> acc
  | h :: t ->
      let modified_h = modify_h h in
      location_smoothing t (modified_h :: acc)

(*[array_filter] finds the entities that satisfy a predicate and returns
  a list of those entities *)
let array_filter pred (state : Common.world_state) =
  Array.fold_left
    (fun acc (t : Common.entity option) ->
      match t with
      | Some t -> if pred t then t :: acc else acc
      | None -> acc)
    [] state.data

let get_local (state : Common.world_state) (x : float) (y : float) =
  let inside_circle (t : Common.entity) =
    inside_circle t.x t.y x y radius
  in
  Mutex.lock state.mutex;
  let local_object = array_filter inside_circle state in
  Mutex.unlock state.mutex;
  location_smoothing local_object []

let xy (state : Common.world_state) =
  let uuid = state.uuid in
  match !uuid with
  | Some uuid -> (
      let candidates =
        array_filter (fun entity -> entity.uuid = uuid) state
      in
      match candidates with
      | [] -> None
      | h::_ -> Some (h.x, h.y))
  | None -> None

let get_player_xy (state : Common.world_state) =
  Mutex.lock state.mutex;
  let location = xy state in
  Mutex.unlock state.mutex;
  location
