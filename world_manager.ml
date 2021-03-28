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
  if d ** 2. <= r ** 2. then true else false

(*[modify_h] modifies a given entity by moving its x,y position given
  vx, vy, time_sent_over and current system time. None of the other
  parameeters of entity are changed.*)
let modify_h (h : Common.entity) =
  {
    Common.uuid = h.uuid;
    x = h.x +. (h.vx *. (Unix.gettimeofday () -. h.time_sent_over) /. 2.);
    y = h.y +. (h.vy *. (Unix.gettimeofday () -. h.time_sent_over) /. 2.);
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

(*[array_filter] finds the entities that local (that is to say within
  the specified radius) to x,y position and returns a list of those
  "local" entities *)
let array_filter (state : Common.world_state) x y =
  Array.fold_left
    (fun acc (t : Common.entity option) ->
      match t with
      | Some t ->
          let inside_circle_value = inside_circle t.x t.y x y radius in
          if inside_circle_value = true then t :: acc else acc
      | None -> acc)
    [] state.data

let get_local (state : Common.world_state) (x : float) (y : float) =
  Mutex.lock state.mutex;
  let local_object = array_filter state x y in
  Mutex.unlock state.mutex;
  location_smoothing local_object []
