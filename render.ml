(** [string_combiner graphic anim anim_frame] is used to attach the
    [graphic] string, [anim] string, [anim_frame], and finally ".png"
    together to form the correct graphic string to look for in the image
    hashmap. *)
let string_combiner graphic anim anim_frame =
  graphic ^ anim ^ string_of_int anim_frame ^ ".png"

(** [anim_decider right idle attack weapon] is used to pick which
    graphic to call based on if the player is facing right or left with
    [right], then if the player is idle with [idle] and then if the
    player should be doing an attack animation with [attack], and
    finally what weapon they should be using based on [weapon]. *)
let anim_decider right idle attack weapon =
  if weapon = "" then
    if right && attack then "_attack_right_"
    else if (not right) && attack then "_attack_left_"
    else if right && idle then "_idle_right_"
    else if right && not idle then "_walk_right_"
    else if (not right) && idle then "_idle_left_"
    else if (not right) && not idle then "_walk_left_"
    else "error"
  else if right && attack then "_" ^ weapon ^ "_attack_right_"
  else if (not right) && attack then "_" ^ weapon ^ "_attack_left_"
  else if right && idle then "_" ^ weapon ^ "_idle_right_"
  else if right && not idle then "_" ^ weapon ^ "_walk_right_"
  else if (not right) && idle then "_" ^ weapon ^ "_idle_left_"
  else if (not right) && not idle then "_" ^ weapon ^ "_walk_left_"
  else "error"

(** [blit_image pos_rect hashmap screen graphic] renders [graphic] on
    [screen] at [pos_rect] by first finding the image associated with
    [graphic] in [hashmap] and then rendering it. *)
let blit_image pos_rect hashmap screen (graphic : string) =
  try
    let texture = Hashtbl.find hashmap (graphic ^ ".png") in
    Sdlvideo.blit_surface ~dst_rect:pos_rect ~src:texture ~dst:screen ();
    ()
  with Not_found ->
    Sdlvideo.blit_surface ~dst_rect:pos_rect
      ~src:(Hashtbl.find hashmap "error.png")
      ~dst:screen ();
    ()

(** [draw_health screen hashmap x y health max_health] renders the
    entities current [health] and converts it to a number between 0 and
    5 by comparing it to its [max_health] and multiplying by 5. It then
    uses that number to render the correct health bar with the right
    amount of health left. *)
let draw_health screen hashmap x y health max_health =
  let health_num = max (health /. max_health *. 5.0) 0. in
  let health_pos_rect = Sdlvideo.rect x (y - 15) 1000 1000 in
  blit_image health_pos_rect hashmap screen
    ("health_bar_" ^ string_of_int (int_of_float health_num))

(** [score_to_string screen hashmap x y num len] renders [num] which
    represents one character in a players score. This is called by
    draw_score for each character of a players score. *)
let score_to_string screen hashmap x y (num : char) len =
  let score_pos_rect =
    Sdlvideo.rect (x - (12 * len)) (y - 17) 100 100
  in
  blit_image score_pos_rect hashmap screen (String.make 1 num)

(** [draw_score screen hashmap x y score] renders a players score on
    screen at the correct location. It does this by converting a players
    score to a string of ints 0-9 and draws them. This is done because
    we only have the numbers 0-9 as graphics so we convert any number
    bigger then 9 to its individual characters and draw them
    individually.

    Example:

    - "1"-> "1"
    - "10" -> "1", "0"
    - "843" -> "8", "4", "3" *)
let draw_score screen hashmap x y score =
  let score_string = string_of_int score in
  let offset = ref 0 in
  String.iter
    (fun num ->
      score_to_string screen hashmap (x + !offset) y num
        (String.length score_string);
      offset := 12 + !offset)
    score_string

(** [rect_helper x] is used to calculate the x coordinate that an item
    should be rendered at on screen. *)
let rect_helper offset x y = Sdlvideo.rect (x + (offset * 94)) y 100 100

(** [draw_items screen hashmap item_list] renders the items in
    [item_list] on the screen*)
let draw_items screen hashmap (item_list : Common.weapon list) x y =
  let item_name_list =
    List.map (fun (x : Common.weapon) -> x.name) item_list
  in
  List.iteri
    (fun offset weapon_name ->
      let images =
        try Hashtbl.find hashmap (weapon_name ^ ".png")
        with Not_found -> Hashtbl.find hashmap "error.png"
      in
      Sdlvideo.blit_surface
        ~dst_rect:(rect_helper offset x y)
        ~src:images ~dst:screen ())
    item_name_list

(** [draw_inventory screen hashmap] renders the inventory at the correct
    location on screen. *)
let draw_inventory screen hashmap pos_rect =
  Sdlvideo.blit_surface ~dst_rect:pos_rect
    ~src:(Hashtbl.find hashmap "inventory.png")
    ~dst:screen ()

(** [weapon_check weapon_list] checks if a player has a weapon equipped
    and returns either "" or the weapons name. *)
let weapon_check (weapon_list : Common.weapon list) =
  if List.length weapon_list = 0 then ""
  else
    let weapon = List.hd weapon_list in
    weapon.name

(** [draw_health_score_inventory entity hashmap screen x y uuid] is
    called by image_getter_render and is used to render health at the
    correct location of [entity] and check if [entity] is a player and
    if so then it will render score and inventory as well. *)
let draw_health_score_inventory
    (entity : Common.entity)
    hashmap
    screen
    x
    y
    uuid =
  let inv_position_rect = Sdlvideo.rect 819 950 100 100 in
  if entity.kind = Player || entity.kind = Ai then
    draw_health screen hashmap x y entity.health entity.max_health;
  if entity.kind = Player then
    draw_score screen hashmap x y entity.points;
  match uuid with
  | Some uuid ->
      if entity.uuid = uuid then (
        draw_inventory screen hashmap inv_position_rect;
        draw_items screen hashmap entity.inventory 819 950 )
  | None -> ()

let trader_inventory : Common.weapon list =
  [
    { name = "sword"; range = 0.0; damage = 0.0; cooldown = 0.0 };
    { name = "fists"; range = 0.0; damage = 0.0; cooldown = 0.0 };
    (*{ name = "hand of judgement"; range = 0.0; damage = 0.0; cooldown
      = 0.0; };*)
  ]

let draw_shop_sign hashmap screen x y =
  let shop_position_rect = Sdlvideo.rect (x - 94) (y + 35) 100 100 in
  blit_image shop_position_rect hashmap screen "shop96"

let draw_trader_inventory
    (entity : Common.entity)
    hashmap
    screen
    p_x
    p_y
    t_x
    t_y
    r_x
    r_y =
  if ((p_x - t_x) * (p_x - t_x)) + ((p_y - t_y) * (p_y - t_y)) < 100000
  then (
    let inv_position_rect =
      Sdlvideo.rect (r_x - 94) (r_y + 100) 100 100
    in
    draw_inventory screen hashmap inv_position_rect;
    draw_items screen hashmap trader_inventory (r_x - 94) (r_y + 100);
    draw_shop_sign hashmap screen r_x r_y )
  else
    let norm =
      sqrt
        (float_of_int
           (((t_x - p_x) * (t_x - p_x)) + ((t_y - p_y) * (t_y - p_y))))
    in
    let position_rect =
      Sdlvideo.rect
        ( int_of_float
            ((float_of_int (t_x - p_x) /. norm *. 100.) +. (1920. /. 2.))
        - 48 )
        ( int_of_float
            ((float_of_int (t_y - p_y) /. norm *. 100.) +. (1080. /. 2.))
        - 48 )
        100 100
    in
    blit_image position_rect hashmap screen "trader_idle_right_0"

let find_source (entity : Common.entity) hashmap screen anim_frame time
    =
  try
    let weapon_name = weapon_check entity.inventory in
    Hashtbl.find hashmap
      (string_combiner entity.graphic
         (anim_decider
            (not entity.last_direction_moved)
            (entity.vx = 0.0 && entity.vy = 0.0)
            (time -. entity.last_attack_time < 0.55)
            weapon_name)
         anim_frame)
  with Not_found -> (
    try Hashtbl.find hashmap (entity.graphic ^ ".png")
    with Not_found -> Hashtbl.find hashmap "error.png" )

(** [image_getter_render entity hashmap screen anim_frame (w, h) (x, y)
    uuid] is used to render all entities and health, inventory, and
    score (score and inventory are rendered only for players). Also
    renders each frame according to the animation frame they are on. *)
let image_getter_render
    (entity : Common.entity)
    hashmap
    screen
    anim_frame
    (w, h)
    (x, y)
    uuid =
  let x_coord = int_of_float entity.x - 48 + (w / 2) - int_of_float x in
  let y_coord = int_of_float entity.y - 48 + (h / 2) - int_of_float y in
  let position_rect = Sdlvideo.rect x_coord y_coord 100 100 in
  let now = Unix.gettimeofday () in
  let source = find_source entity hashmap screen anim_frame now in
  Sdlvideo.blit_surface ~dst_rect:position_rect ~src:source ~dst:screen
    ();
  draw_health_score_inventory entity hashmap screen x_coord y_coord uuid;
  if entity.kind = Merchant then
    draw_trader_inventory entity hashmap screen (int_of_float x)
      (int_of_float y) (int_of_float entity.x) (int_of_float entity.y)
      x_coord y_coord

(** [generate_y height background_size x y acc] generates the vertical
    grid components such as (1,2) (1,3) (2,2) (2,3). *)
let rec generate_y height background_size x y acc =
  match height with
  | 0 -> acc
  | _ ->
      generate_y (height - 1) background_size x (y +. background_size)
        (acc @ [ (x, y) ])

(** [generate_grid width height background_size x y acc] generates a
    grid of list of (float * float) coordinates based on
    [background_size] [width] and [height]. *)
let rec generate_grid width height background_size x y acc =
  match width with
  | 0 -> acc
  | _ ->
      let new_acc = generate_y height background_size x y acc in
      generate_grid (width - 1) height background_size
        (x +. background_size) y new_acc

(** [draw_tiles hashmap screen x y] renders the tiles at position [x]
    [y] on [screen]. *)
let draw_tiles hashmap screen x y =
  let map_position_rect =
    Sdlvideo.rect (int_of_float x) (int_of_float y) 100 100
  in
  Sdlvideo.blit_surface ~dst_rect:map_position_rect
    ~src:(Hashtbl.find hashmap "map.png")
    ~dst:screen ()

(** [draw_tile screen hashmap grid x y] calls draw_tiles in a grid shape
    that is passed in by [grid]. *)
let rec draw_tile screen hashmap (grid : (float * float) list) x y =
  List.iter
    (fun (w, h) -> draw_tiles hashmap screen (w +. x) (h +. y))
    grid

(** [draw_background screen hashmap grid x y size] draws the background *)
let draw_background screen hashmap grid x y size =
  let new_x = mod_float (-1.0 *. x) (512. *. size) in
  let new_y = mod_float (-1.0 *. y) (512. *. size) in
  draw_tile screen hashmap grid new_x new_y

(** [check_height height] checks the height and keeps it in bounds of
    the screen *)
let check_height height =
  if height < 0 then 0 else if height > 1060 then 1060 else height

(** [check_width width] checks the width and keeps it in bounds of the
    screen *)
let check_width width =
  if width < 0 then 0 else if width > 1800 then 1800 else width

(** [draw_golden_camel_pointer s h x y] takes in screen, hashmap, and, x
    and y coordinates and draws the golden camel pointer that always
    points towards the center of the screen. *)
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

(** [run world map] takes in world state [world] and hashmap [map] and
    renders the world map and every entity in world along with its
    health, score, and inventory if its a player and its health if its
    not a player with the associated graphics in the hashmap. *)
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
      World_manager.get_player_xy world_state
      |> Option.value ~default:(0., 0.)
    in
    let player_uuid = World_manager.get_player_uuid world_state in
    draw_background screen hashmap grid x y 6.;
    let time_begin = Sdltimer.get_ticks () in
    let anim_frame = (Sdltimer.get_ticks () - start_time) / 150 in
    List.mapi
      (fun index (entity : Common.entity) ->
        image_getter_render entity hashmap screen
          ((anim_frame + index) mod 4)
          (w, h) (x, y) player_uuid)
      world
    |> ignore;
    draw_golden_camel_pointer screen hashmap x y;
    Sdlvideo.flip screen;
    let time_end = Sdltimer.get_ticks () in
    Sdltimer.delay (max ((1000 / 60) - (time_end - time_begin)) 0)
  done
