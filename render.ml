let string_combiner graphic anim anim_frame =
  graphic ^ anim ^ string_of_int anim_frame ^ ".png"

let anim_decider right idle =
  (*this branch below is never called right now because there is no way
    of storing which way it was facing in previous frames before it
    stopped*)
  if right && idle then "_idle_right_"
  else if right && not idle then "_walk_right_"
  else if (not right) && idle then "_idle_left_"
  else if (not right) && not idle then "_walk_left_"
  else "error"

let blit_image pos_rect hashmap screen (graphic : string) =
  Sdlvideo.blit_surface ~dst_rect:pos_rect
    ~src:(Hashtbl.find hashmap graphic)
    ~dst:screen ();
  ()

let draw_health screen hashmap x y health max_health =
  let health_pos_rect = Sdlvideo.rect x (y - 15) 1000 1000 in
  blit_image health_pos_rect hashmap screen
    ( "health_bar_"
    ^ string_of_int (int_of_float (health /. max_health *. 5.0))
    ^ ".png" )

(*code below is for when have 100 possibilities*)
(*if player_health <= 100 then blit_image health_position_rect hashmap
  screen “health_bar_” ^ (string_of_int (int_of_float (player_health)))
  else blit_image health_position_rect hashmap screen “health_bar_” ^
  (string_of_int (int_of_float (player_health /. 100)))*)

(*let rec draw_health screen hashmap x y acc row_count row = let
  health_position_rect = Sdlvideo.rect x (y - 5 - (16 * row)) 100 100 in
  if acc < 1.0 then () else if row_count = 4 then ( blit_image
  health_position_rect hashmap screen "heart_32x32.png"; draw_health
  screen hashmap (x - 64) y (acc -. 1.0) (row_count + 1) (row + 1); () )
  else ( blit_image health_position_rect hashmap screen
  "heart_32x32.png"; draw_health screen hashmap (x + 16) y (acc -. 1.0)
  (row_count + 1) row; () )*)

let image_getter_render
    (entity : Common.entity)
    hashmap
    screen
    anim_frame
    (w, h)
    (x, y) =
  let health = entity.health in
  let x_coord = int_of_float entity.x - 48 + (w / 2) - int_of_float x in
  let y_coord = int_of_float entity.y - 48 + (h / 2) - int_of_float y in
  let position_rect = Sdlvideo.rect x_coord y_coord 100 100 in
  let source =
    try
      Hashtbl.find hashmap
        (string_combiner entity.graphic
           (anim_decider
              (not entity.last_direction_moved)
              (entity.vx = 0.0 && entity.vy = 0.0))
           anim_frame)
    with Not_found -> Hashtbl.find hashmap "error"
  in
  Sdlvideo.blit_surface ~dst_rect:position_rect ~src:source ~dst:screen
    ();
  draw_health screen hashmap x_coord y_coord health 100.0

(*NEED max_health*)

let draw_background screen hashmap x y =
  let map_position_rect =
    Sdlvideo.rect
      (int_of_float ((-1.0 *. x) -. (8192. /. 2.)))
      (int_of_float ((-1.0 *. y) -. (8192. /. 2.)))
      100 100
  in
  Sdlvideo.blit_surface ~dst_rect:map_position_rect
    ~src:(Hashtbl.find hashmap "map.png")
    ~dst:screen ();
  ()

(**[run] takes in world state and hashmap and renders the world map and
   every entity in world with the associated graphic in hashmap*)
let run (world_state : Common.world_state) hashmap =
  let start_time = Sdltimer.get_ticks () in
  let w, h = (1920, 1080) in
  let screen = Sdlvideo.set_video_mode w h [ `DOUBLEBUF ] in
  Thread.create Input_handler.key_checker world_state |> ignore;
  while true do
    let world = World_manager.get_local world_state 400.0 300.0 in
    let x, y =
      (* TODO: bad *)
      World_manager.get_player_xy world_state
      |> Option.value ~default:(0., 0.)
    in
    draw_background screen hashmap x y;
    let time_begin = Sdltimer.get_ticks () in
    (*TODO unsync animations?*)
    let anim_frame = (Sdltimer.get_ticks () - start_time) / 150 mod 4 in
    List.map
      (fun e ->
        image_getter_render e hashmap screen anim_frame (w, h) (x, y))
      world
    |> ignore;
    Sdlvideo.flip screen;
    let time_end = Sdltimer.get_ticks () in
    Sdltimer.delay (max ((1000 / 60) - (time_end - time_begin)) 0)
  done
