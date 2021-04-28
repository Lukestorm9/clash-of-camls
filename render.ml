let string_combiner graphic anim anim_frame =
  graphic ^ anim ^ string_of_int anim_frame ^ ".png"

let anim_decider right idle attack =
  (*this branch below is never called right now because there is no way
    of storing which way it was facing in previous frames before it
    stopped*)
  if right && attack then "_fist_attack_right_"
  else if (not right) && attack then "_fist_attack_left_"
  else if right && idle then "_idle_right_"
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
  let health_num = max (health /. max_health *. 5.0) 0. in
  let health_pos_rect = Sdlvideo.rect x (y - 15) 1000 1000 in
  blit_image health_pos_rect hashmap screen
    ("health_bar_" ^ string_of_int (int_of_float health_num) ^ ".png")

let image_getter_render
    (entity : Common.entity)
    hashmap
    screen
    anim_frame
    (w, h)
    (x, y) =
  let x_coord = int_of_float entity.x - 48 + (w / 2) - int_of_float x in
  let y_coord = int_of_float entity.y - 48 + (h / 2) - int_of_float y in
  let position_rect = Sdlvideo.rect x_coord y_coord 100 100 in
  let now = Unix.gettimeofday () in
  let source =
    try
      Hashtbl.find hashmap
        (string_combiner entity.graphic
           (anim_decider
              (not entity.last_direction_moved)
              (entity.vx = 0.0 && entity.vy = 0.0)
              (now -. entity.last_attack_time < 0.55))
           anim_frame)
    with Not_found -> (
      try Hashtbl.find hashmap (entity.graphic ^ ".png")
      with Not_found -> Hashtbl.find hashmap "error.png" )
  in
  Sdlvideo.blit_surface ~dst_rect:position_rect ~src:source ~dst:screen
    ();
  if entity.kind = Player || entity.kind = Ai || entity.kind = Camel
  then
    draw_health screen hashmap x_coord y_coord entity.health
      entity.max_health

let rec generate_y height background_size x y acc =
  match height with
  | 0 -> acc
  | _ ->
      generate_y (height - 1) background_size x (y +. background_size)
        (acc @ [ (x, y) ])

let rec generate_grid width height background_size x y acc =
  match width with
  | 0 -> acc
  | _ ->
      let new_acc = generate_y height background_size x y acc in
      generate_grid (width - 1) height background_size
        (x +. background_size) y new_acc

let draw_tiles hashmap screen x y =
  let map_position_rect =
    Sdlvideo.rect (int_of_float x) (int_of_float y) 100 100
  in
  Sdlvideo.blit_surface ~dst_rect:map_position_rect
    ~src:(Hashtbl.find hashmap "map.png")
    ~dst:screen ()

let rec draw_tile screen hashmap (grid : (float * float) list) x y =
  List.iter
    (fun (w, h) -> draw_tiles hashmap screen (w +. x) (h +. y))
    grid

let draw_background screen hashmap grid x y size =
  let new_x = mod_float (-1.0 *. x) (512. *. size) in
  let new_y = mod_float (-1.0 *. y) (512. *. size) in
  draw_tile screen hashmap grid new_x new_y

let draw_inventory screen hashmap =
  let inv_position_rect = Sdlvideo.rect 819 950 100 100 in
  Sdlvideo.blit_surface ~dst_rect:inv_position_rect
    ~src:(Hashtbl.find hashmap "inventory.png")
    ~dst:screen ()

let draw_items screen hashmap item_list =
  let item_position_rect = Sdlvideo.rect 819 950 100 100 in
  Sdlvideo.blit_surface ~dst_rect:item_position_rect
    ~src:(Hashtbl.find hashmap "fists.png")
    ~dst:screen ()

let check_height height =
  if height < 0 then 0 else if height > 1060 then 1060 else height

let check_width width =
  if width < 0 then 0 else if width > 1800 then 1800 else width

let draw_golden_camel_pointer screen hashmap x y =
  if (x > 500. || x < -500.) || y > 500. || y < -500. then
    let width =
      int_of_float
        (960. +. (-1. *. x *. 540. /. sqrt ((x *. x) +. (y *. y))))
    in
    let height =
      int_of_float
        (540. +. (-1. *. y *. 540. /. sqrt ((x *. x) +. (y *. y))))
    in
    let golden_camel_icon_position_rect =
      Sdlvideo.rect width height 100 100
    in
    Sdlvideo.blit_surface ~dst_rect:golden_camel_icon_position_rect
      ~src:(Hashtbl.find hashmap "golden_camel_icon.png")
      ~dst:screen ()

(**[run] takes in world state and hashmap and renders the world map and
   every entity in world with the associated graphic in hashmap*)
let run (world_state : Common.world_state) hashmap =
  let start_time = Sdltimer.get_ticks () in
  let w, h = (1920, 1080) in
  let screen = Sdlvideo.set_video_mode w h [ `DOUBLEBUF ] in
  Thread.create Input_handler.key_checker world_state |> ignore;
  let grid =
    generate_grid 5 5 3072. (-2.5 *. 3072.) (-2.5 *. 3072.) []
  in
  while true do
    let world = World_manager.get_local world_state 400.0 300.0 in
    let x, y =
      (* TODO: bad *)
      World_manager.get_player_xy world_state
      |> Option.value ~default:(0., 0.)
    in
    draw_background screen hashmap grid x y 6.;
    let time_begin = Sdltimer.get_ticks () in
    (*TODO unsync animations?*)
    let anim_frame = (Sdltimer.get_ticks () - start_time) / 150 in
    List.mapi
      (fun i (e : Common.entity) ->
        image_getter_render e hashmap screen
          ((anim_frame + i) mod 4)
          (w, h) (x, y))
      world
    |> ignore;
    draw_inventory screen hashmap;
    draw_items screen hashmap [];
    draw_golden_camel_pointer screen hashmap x y;
    Sdlvideo.flip screen;
    let time_end = Sdltimer.get_ticks () in
    Sdltimer.delay (max ((1000 / 60) - (time_end - time_begin)) 0)
  done
