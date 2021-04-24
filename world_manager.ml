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
    h with
    x = h.x +. (h.vx *. (Unix.gettimeofday () -. h.time_sent_over));
    y = h.y +. (h.vy *. (Unix.gettimeofday () -. h.time_sent_over));
  }

let rec location_smoothing
    (world : Common.entity list)
    (acc : Common.entity list) : Common.entity list =
  match world with
  | [] -> acc
  | h :: t ->
      let modified_h = modify_h h in
      location_smoothing t (modified_h :: acc)

let get_local (state : Common.world_state) (x : float) (y : float) =
  let inside_circle (t : Common.entity) =
    inside_circle t.x t.y x y radius
  in
  Mutex.lock state.mutex;
  let local_object = Common.array_filter inside_circle state.data in
  Mutex.unlock state.mutex;
  location_smoothing local_object []

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
          let corrected = modify_h h in
          Some (corrected.x, corrected.y))
  | None -> None

let get_player_xy (state : Common.world_state) =
  Mutex.lock state.mutex;
  let location = xy state in
  Mutex.unlock state.mutex;
  location


(**Inventory system: keep track which items.*)
