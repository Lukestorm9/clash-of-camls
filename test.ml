(** This this section outlines the test plan.

    1. Parts of the system were automatically tested by OUnit vs.
    manually tested: We test the [World Manager] functions using OUnit
    testing. [Render], [Client], and parts of [Client] are te test
    manually as there is good way to test the functions those modules.
    [World Manager] has functions that well-suited for testing using the
    OUnit suite, thus it was not manually tested.e

    2. Modules were tested by OUnit and how test cases were developed
    (black box, glass box, randomized, etc.): The tests for
    [World Manager] is tests for black box testing. The tests attempt to
    ensure that the fucntions to not return outputs that are unintended.
    The test for [World Manager] attempt to see that the out is as
    intended by the documentation.

    3. An argument for why the testing approach demonstrates the
    correctness of the system: *)

open OUnit2
open World_manager
open Model

(** [print_entity_type] prints the entity type*)
let print_entity_type (kind : Common.entity_type) =
  match kind with
  | Physik -> "Physik"
  | Ai -> "Ai"
  | Player -> "Player"
  | Merchant -> "Mercant"
  | Camel (Some i) -> "Camel of int " ^ string_of_int i
  | _ ->
      failwith
        "Not possible Kind. Please check Commmon.ml for updates to \
         entity_type."

(** [print_weapons] prints the weapon list*)
let rec print_weapons (weapons_list : Common.weapon list) acc =
  match weapons_list with
  | [] -> "[\n" ^ acc ^ " ]"
  | h :: t ->
      let weapon =
        "{ " ^ "name = " ^ h.name ^ ", " ^ "range = "
        ^ string_of_float h.range ^ ", " ^ "damage = "
        ^ string_of_float h.damage
        ^ ", " ^ "cooldown = "
        ^ string_of_float h.cooldown
        ^ " }" ^ ";\n"
      in
      print_weapons t (weapon ^ acc)

(** [print_entity] prints the all of the entity information *)
let print_entity (h : Common.entity) =
  "{ " ^ "kind = "
  ^ print_entity_type h.kind
  ^ ", " ^ "uuid = " ^ string_of_int h.uuid ^ ", " ^ "x = "
  ^ string_of_float h.x ^ ", " ^ "y = " ^ string_of_float h.y ^ ", "
  ^ "vx = " ^ string_of_float h.vx ^ ", " ^ "vy = "
  ^ string_of_float h.vy ^ ", " ^ "time_sent_over = "
  ^ string_of_float h.time_sent_over
  ^ ", " ^ "graphic = " ^ h.graphic ^ ", " ^ "health = "
  ^ string_of_float h.health
  ^ ", " ^ "max_health = "
  ^ string_of_float h.max_health
  ^ ", " ^ "last_direction_moved = "
  ^ string_of_bool h.last_direction_moved
  ^ ", " ^ "inventory = "
  ^ print_weapons h.inventory ""
  ^ ", " ^ "points = " ^ string_of_int h.points ^ ", "
  ^ "last_attack_time = "
  ^ string_of_float h.last_attack_time
  ^ " }" ^ ";\n"

(** [print_entity_list] prints the given entity_list. All information
    about each entity will be printed. An entity is defined in
    Common.mli*)
let rec print_entity_list acc (entity_list : Common.entity list) =
  match entity_list with
  | [] -> "[\n" ^ acc ^ " ]"
  | h :: t -> print_entity_list (print_entity h ^ acc) t

let rec print_int_entity_list acc (pair : (int * Common.entity) list) =
  match pair with
  | [] -> acc
  | (i, e) :: t ->
      print_int_entity_list
        (acc ^ "( " ^ string_of_int i ^ ", " ^ print_entity e ^ " )")
        t

(** [compare_indexes] compares that the indexes outputed are same
    regarless of the order they are in expected or calculated. The
    sorting process does not remove duplicate values. *)
let compare_indexes
    (expected_enemy_lst : (int * Common.entity) list)
    (calulated_enemy_lst : (int * Common.entity) list) =
  let exp_indices =
    expected_enemy_lst |> List.map fst |> List.sort compare
  in
  let cal_indices =
    calulated_enemy_lst |> List.map fst |> List.sort compare
  in
  exp_indices = cal_indices

(** [print_float_pair_option] prints and float option pair. If the
    option pair is some then it prints the float pair else if None then
    this function prints the word "None"*)
let print_float_pair_option (pair : (float * float) option) =
  match pair with
  | Some (s, t) ->
      "( " ^ string_of_float s ^ ", " ^ string_of_float t ^ " )"
  | None -> "None"

let boundary_logic (cal : Common.entity) (exp : Common.entity) =
  let boundary = 0.5 in
  cal.x +. boundary >= exp.x
  && cal.x -. boundary <= exp.x
  && cal.y +. boundary >= exp.y
  && cal.y -. boundary <= exp.y
  && cal.last_attack_time +. boundary >= exp.last_attack_time
  && cal.last_attack_time -. boundary <= exp.last_attack_time
  && cal.kind = cal.kind && cal.uuid = exp.uuid
  && cal.time_sent_over = exp.time_sent_over
  && cal.max_health = exp.max_health
  && cal.health = exp.health
  && cal.last_direction_moved = exp.last_direction_moved
  && cal.graphic = exp.graphic
  && cal.inventory = exp.inventory
  && cal.points = exp.points

let check_within_bounds exp cal : bool = boundary_logic cal exp

(** [check_within_bounds_helper] is helper function for check_within
    bounds.*)
let rec check_within_bounds_list_helper
    (expected_world : Common.entity list)
    (calculated_world : Common.entity list) : bool =
  match (expected_world, calculated_world) with
  | [], [] -> true
  | exp :: t, cal :: m ->
      if boundary_logic cal exp then check_within_bounds_list_helper t m
      else false
  | _ :: _, [] | [], _ :: _ -> false

(** [check_within_bounds_float_pairs_helper] is helper function for
    check_within_bounds_float_pairs.*)
let check_within_bounds_float_pair_helper
    (float_pair_expected : (float * float) option)
    (float_pair_given : (float * float) option)
    (boundary : float) : bool =
  match (float_pair_expected, float_pair_given) with
  | Some (exp_a, exp_b), None -> false
  | None, Some _ -> false
  | None, None -> true
  | Some (exp_a, exp_b), Some (cal_a, cal_b) ->
      if
        cal_a +. boundary >= exp_a
        && cal_a -. boundary <= exp_a
        && cal_b +. boundary >= exp_b
        && cal_b -. boundary <= exp_b
      then true
      else false

(** [check_within_bounds_float_pair] checks if each float option pair is
    within bounds. Check documentation for [check_within_bounds_list] to
    understand how within bounds is defined.*)
let check_within_bounds_float_pair
    (float_pair_expected : (float * float) option)
    (float_pair_given : (float * float) option) : bool =
  let boundary = 0.5 in
  check_within_bounds_float_pair_helper float_pair_expected
    float_pair_given boundary

(** [check_within_bounds_list] checks if the calculated_world's
    (calculated_word is the original world passed to
    World_manager.get_local) x,y pair is within a bounded number with
    expected_world's x,y pair, and the functions does for all entites.
    The bounds can be changed by changing bounded. Currently the bounded
    is set to 0.5
    *******************************************************************************
    Ex with only one entity: expect_world's x,y: (0, 0) output_world's
    x,y: (1, 0.5) -> check_within_bounds_list produces false because
    output_world's 1 is out of bounds given a bounds of 0.5. The
    acceptable bounds for x,y for -0.5 and +0.5 for this example.*)
let rec check_within_bounds_list
    (expected_world : Common.entity list)
    (calculated_world : Common.entity list) : bool =
  check_within_bounds_list_helper expected_world calculated_world

(******************************************************************************
                          HELPER FUNCTIONS END                                    
 ******************************************************************************)

let world_manager_get_local_tests
    (name : string)
    (state : Common.world_state)
    (x : float)
    (y : float)
    (expected_output : Common.entity list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (World_manager.get_local state x y)
    ~cmp:check_within_bounds_list ~printer:(print_entity_list "")

let world_manager_get_player_xy_tests
    (name : string)
    (state : Common.world_state)
    (expected_output : (float * float) option) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (World_manager.get_player_xy state)
    ~cmp:check_within_bounds_float_pair ~printer:print_float_pair_option

let model_get_local_enemies_tests
    (name : string)
    state
    (entity : Common.entity)
    radius
    direction
    (expected_output : (int * Common.entity) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Model.get_local_enemies state entity radius direction)
    ~cmp:compare_indexes
    ~printer:(print_int_entity_list "")

let model_process_movement_tests
    (name : string)
    (entity : Common.entity)
    (d : Common.direction)
    (expected_output : Common.entity) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Model.process_movement entity d)
    ~printer:print_entity

let model_process_attack_tests
    (name : string)
    (state : Common.serv_state)
    (entity : Common.entity)
    (d : Common.direction)
    (expected_output : Common.entity) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Model.process_attack state entity d)
    ~cmp:check_within_bounds ~printer:print_entity

let model_follow_tests
    (name : string)
    (state : Common.serv_state)
    (entity : Common.entity)
    (expected_output : Common.entity) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Model.follow state entity)
    ~printer:print_entity

let model_try_acquire_imprint_tests
    (name : string)
    (state : Common.serv_state)
    (entity : Common.entity)
    (expected_output : Common.entity) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Model.try_acquire_imprint state entity)
    ~printer:print_entity

let model_apply_enemy_step_tests
    (name : string)
    (state : Common.serv_state)
    (entity : Common.entity)
    (expected_output : Common.entity) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Model.apply_enemy_step state entity)
    ~cmp:check_within_bounds ~printer:print_entity

let world_state_maker ~data ~mutex ~uuid ~user_command :
    Common.world_state =
  { data; mutex; uuid; user_command }

let world_state_maker_server ~data ~points_gathered ~mutex :
    Common.serv_state =
  { data; points_gathered; mutex }

let entity_maker
    ~kind
    ~uuid
    ~x
    ~y
    ~vx
    ~vy
    ~time_sent_over
    ~graphic
    ~health
    ~last_direction_moved
    ~inventory
    ~points
    ~last_attack_time : Common.entity =
  {
    kind;
    uuid;
    x;
    y;
    vx;
    vy;
    time_sent_over;
    graphic;
    health;
    max_health = health;
    last_direction_moved;
    inventory;
    points;
    last_attack_time;
  }

let (fist : Common.weapon) =
  {
    name = "fists";
    range = sqrt 18100.;
    damage = 20.0;
    cooldown = 0.25;
  }

let non_moving_entity_at_origin =
  entity_maker Common.Player 1 0. 0. 0. 0. 0. "camel" 0. false [ fist ]
    0 0.

let non_moving_camel_at_origin =
  entity_maker (Common.Camel (Some 0)) 1 0. 0. 0. 0. 0. "camel" 0. false
    [ fist ] 0 0.

let moving_entity_at_origin =
  entity_maker Common.Player 2 0. 0. 5. 5. 1. "camel" 0. false [ fist ]
    0 0.

let moving_camel_near_entity_3 =
  entity_maker (Common.Camel (Some 1)) 2 (-100.) 91. 5. 5. 1. "camel" 0.
    false [ fist ] 0 0.

let moving_entity_at_origin' =
  entity_maker Common.Player 2
    (0. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    (0. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    5. 5. 1. "camel" 0. false [ fist ] 0 0.

let entity_3 =
  entity_maker Common.Player 3 (-100.) 90. 5. 5. 1. "camel" 0. false
    [ fist ] 10 0.

let entity_3' =
  entity_maker Common.Player 3
    (-100. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    (90. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    5. 5. 1. "camel" 0. false [ fist ] 0 0.

let entity_4 =
  entity_maker Common.Player 4 0. 3. 5. 5. 1. "camel" 0. true [ fist ] 0
    0.

let entity_5_center =
  entity_maker Common.Player 5 10. (-2.) 5. 5. 1. "camel" 0. true
    [ fist ] 0 0.

let entity_6_down =
  entity_maker Common.Player 6 12. (-5.) 5. 5. 1. "camel" 0. true
    [ fist ] 10 0.

let entity_6_down' =
  entity_maker Common.Player 6 (-5.) (-20.) 5. 5. 1. "camel" 0. true
    [ fist ] 20 0.

let entity_7_right =
  entity_maker Common.Player 7 40. (-10.) 5. 5. 1. "camel" 0. true
    [ fist ] 20 0.

let entity_7_right' =
  entity_maker Common.Player 7 60. 20. 5. 5. 1. "camel" 0. true [ fist ]
    30 0.

let entity_8_up =
  entity_maker Common.Player 8 8. 4. 5. 5. 1. "camel" 0. true [ fist ]
    30 0.

let entity_8_up' =
  entity_maker Common.Player 8 (-20.) 30. 5. 5. 1. "camel" 0. true
    [ fist ] 40 0.

let entity_9_left =
  entity_maker Common.Player 9 (-11.) (-10.) 5. 5. 1. "camel" 0. true
    [ fist ] 40 0.

let entity_9_left' =
  entity_maker Common.Player 9 5. 1. 5. 5. 1. "camel" 0. true [ fist ]
    50 0.

let empty_world =
  world_state_maker [||] (Mutex.create ()) (ref (Some 0))
    (ref Common.Nothing)

let world_alpha =
  world_state_maker [||] (Mutex.create ()) (ref (Some 0))
    (ref Common.Nothing)

let world_0 =
  world_state_maker
    [| Some non_moving_entity_at_origin |]
    (Mutex.create ()) (ref (Some 0)) (ref Common.Nothing)

let world_1 =
  world_state_maker
    [| Some non_moving_entity_at_origin; Some moving_entity_at_origin |]
    (Mutex.create ()) (ref (Some 1)) (ref Common.Nothing)

let world_2 =
  world_state_maker
    [|
      Some non_moving_entity_at_origin;
      Some moving_entity_at_origin;
      Some entity_3;
    |]
    (Mutex.create ()) (ref (Some 3)) (ref Common.Nothing)

let world_3 =
  world_state_maker
    [|
      Some non_moving_entity_at_origin;
      Some moving_entity_at_origin;
      Some entity_3;
    |]
    (Mutex.create ()) (ref (Some 4)) (ref Common.Nothing)

let world_4 =
  world_state_maker
    [|
      Some non_moving_entity_at_origin;
      Some moving_entity_at_origin;
      Some entity_3;
      Some entity_4;
    |]
    (Mutex.create ()) (ref (Some 4)) (ref Common.Nothing)

let empty_world_server =
  world_state_maker_server [||] (ref 0) (Mutex.create ())

let world_1_server =
  world_state_maker_server
    [|
      Some non_moving_entity_at_origin;
      Some entity_3;
      Some non_moving_camel_at_origin;
      Some moving_camel_near_entity_3;
    |]
    (ref 3) (Mutex.create ())

let world_2_server =
  world_state_maker_server
    [|
      (*0*) Some non_moving_entity_at_origin;
      (*1*) Some moving_entity_at_origin;
      (*2*) Some entity_3;
    |]
    (ref 3) (Mutex.create ())

let world_3_server =
  world_state_maker_server
    [| Some non_moving_entity_at_origin; Some entity_3; Some entity_4 |]
    (ref 3) (Mutex.create ())

let world_4_server =
  world_state_maker_server
    [|
      (*0*) Some entity_5_center;
      (*1*) Some entity_6_down;
      (*2*) Some entity_7_right;
      (*3*) Some entity_8_up;
      (*4*) Some entity_9_left;
      (*5*) Some entity_6_down';
      (*6*) Some entity_7_right';
      (*7*) Some entity_8_up';
      (*8*) Some entity_9_left';
    |]
    (ref 3) (Mutex.create ())

let boundary_point = sqrt ((250000. ** 2.) /. 2.)

let boundary_point_2 = sqrt 18100.

let world_manager_tests =
  [
    world_manager_get_local_tests "Empty world state with (0,0)"
      empty_world 0. 0. [];
    world_manager_get_local_tests "Empty world state with (-1, 0)"
      empty_world (-1.) 0. [];
    world_manager_get_local_tests "Empty world state (-1, -1)"
      empty_world (-1.) (-1.) [];
    world_manager_get_local_tests
      "World state with non_moving_entity_at_origin with (0, 0)" world_0
      0. 0.
      [ non_moving_entity_at_origin ];
    (*Used to check when d^2 = r ^2 -> Include world_0 entity*)
    world_manager_get_local_tests
      "World state with non_moving_entity_at_origin with \
       (boundary_point, boundary_point)"
      world_0 boundary_point boundary_point
      [ non_moving_entity_at_origin ];
    (*d^2 > r^2 thus exclude the entity in world_0*)
    world_manager_get_local_tests
      "World state with non_moving_entity_at_origin with \
       (boundary_point + 0.001, boundary_point + 0.001)"
      world_0
      (boundary_point +. 0.001)
      (boundary_point +. 0.001)
      [];
    (*Testing 2 entities: one moving and one non-moving. Return both
      entities, however the moving entity should be changed to where it
      is predicated to be (i.e. it should apply get_local's location
      smoothing)*)
    (Thread.delay (1. /. 4.);
     world_manager_get_local_tests
       "Using world_1 with (0,0) | Expect all two entities: one \
        non-moving and one moving"
       world_1 0. 0.
       [ non_moving_entity_at_origin; moving_entity_at_origin' ]);
    (*see if it returns "None" when uuid is not found in the given zero
      entities*)
    world_manager_get_player_xy_tests
      "Testing get_player_xy with world_alpha | uuid = 0 and no \
       entities with such uuid"
      world_alpha None;
    (*see if it returns "None" when uuid is not found in one entity*)
    world_manager_get_player_xy_tests
      "Testing get_player_xy with world_0 | uuid = 0 and no entities \
       with such uuid"
      world_0 None;
    (*see if it returns "None" when uuid is not found in the given more
      than one entity*)
    world_manager_get_player_xy_tests
      "Testing get_player_xy with world_3 | uuid = 4 and no matches"
      world_3 None;
    world_manager_get_player_xy_tests
      "Testing get_player_xy with world_1 | uuid = 1 and \
       non_moving_entity_at_origin matches"
      world_1
      (Some (0., 0.));
    (*correctly identify Option pair given multiple options*)
    world_manager_get_player_xy_tests
      "Testing get_player_xy with world_2 | uuid = 3 and entity_3 \
       matches"
      world_2
      (Some (entity_3'.x, entity_3'.y));
  ]

let model_tests =
  [
    model_get_local_enemies_tests
      "Trying to find an enemy on an empty with radius 1."
      empty_world_server non_moving_entity_at_origin 1. Up [];
    model_get_local_enemies_tests
      "Trying to find an enemy on world_2_server | r= 0, entity= \
       non_moving-entity_at_orgin"
      world_2_server non_moving_entity_at_origin 0. Up [];
    model_get_local_enemies_tests
      "Trying to find an enemy on world_2_server | r= 100.0, entity= \
       entity_3"
      world_2_server entity_3 100.0 Up [];
    model_get_local_enemies_tests
      "Trying to find enemies on world_2_server | r=boundary_point_2 - \
       0.001, entity= entity_3"
      world_2_server entity_3
      (boundary_point_2 -. 0.001)
      Up [];
    (*Testing that Left works*)
    model_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Up"
      world_1_server non_moving_entity_at_origin boundary_point_2 Up [];
    model_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Right"
      world_1_server non_moving_entity_at_origin boundary_point_2 Down
      [];
    model_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Down"
      world_1_server non_moving_entity_at_origin boundary_point_2 Right
      [];
    model_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Left"
      world_1_server non_moving_entity_at_origin boundary_point_2 Left
      [ (1, entity_3) ];
    (*Testing that Up works*)
    model_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Up"
      world_3_server non_moving_entity_at_origin boundary_point_2 Up
      [ (2, entity_4) ];
    model_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Down"
      world_3_server non_moving_entity_at_origin boundary_point_2 Down
      [];
    model_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Right"
      world_3_server non_moving_entity_at_origin boundary_point_2 Right
      [];
    model_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Left"
      world_3_server non_moving_entity_at_origin boundary_point_2 Left
      [ (1, entity_3) ];
    (*Testing get_local_enemies with center not origin*)
    (*Testing that Up works*)
    model_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Up"
      world_4_server entity_5_center boundary_point_2 Up
      [ (7, entity_8_up'); (3, entity_8_up) ];
    model_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Down"
      world_4_server entity_5_center boundary_point_2 Down
      [ (5, entity_6_down'); (1, entity_6_down) ];
    model_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Right"
      world_4_server entity_5_center boundary_point_2 Right
      [ (6, entity_7_right'); (2, entity_7_right) ];
    model_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Left"
      world_4_server entity_5_center boundary_point_2 Left
      [ (8, entity_9_left'); (4, entity_9_left) ];
    model_process_movement_tests
      "Process movement on non_moving_entity_at_origin with Common.Left"
      non_moving_entity_at_origin Common.Left
      {
        non_moving_entity_at_origin with
        vx = 300.;
        last_direction_moved = false;
      };
    model_process_movement_tests
      "Process movement on non_moving_entity_at_origin with \
       Common.Right"
      non_moving_entity_at_origin Common.Right
      {
        non_moving_entity_at_origin with
        vx = -300.;
        last_direction_moved = true;
      };
    model_process_movement_tests
      "Process movement on non_moving_entity_at_origin with Common.Up"
      non_moving_entity_at_origin Common.Up
      { non_moving_entity_at_origin with vy = -300. };
    model_process_movement_tests
      "Process movement on non_moving_entity_at_origin with Common.Down"
      non_moving_entity_at_origin Common.Down
      { non_moving_entity_at_origin with vy = 300. };
    model_process_attack_tests
      "Process attack with  empty_world_server \
       non_moving_entity_at_origin 1. Up"
      empty_world_server non_moving_entity_at_origin Up
      {
        non_moving_entity_at_origin with
        last_attack_time = Unix.gettimeofday ();
      };
    model_process_attack_tests
      "Process attack with world_3_server, entity= \
       non_moving_entity_at_origin, d = Left"
      world_3_server non_moving_entity_at_origin Left
      {
        non_moving_entity_at_origin with
        points = 10;
        last_attack_time = Unix.gettimeofday ();
      };
    model_process_attack_tests
      "Process attack with enemies on world_4_server, entity= \
       entity_5_center, d = Up"
      world_4_server entity_5_center Up
      {
        entity_5_center with
        points = 70;
        last_attack_time = Unix.gettimeofday ();
      };
    model_process_attack_tests
      "Process attack with enemies on world_4_server, entity= \
       entity_5_center, d = Down"
      world_4_server entity_5_center Down
      {
        entity_5_center with
        points = 30;
        last_attack_time = Unix.gettimeofday ();
      };
    model_process_attack_tests
      "Process attack with enemies on world_4_server, entity= \
       entity_5_center, d = Right"
      world_4_server entity_5_center Right
      {
        entity_5_center with
        points = 50;
        last_attack_time = Unix.gettimeofday ();
      };
    model_process_attack_tests
      "Process attack with enemies on world_4_server, entity= \
       entity_5_center, d = Left"
      world_4_server entity_5_center Left
      {
        entity_5_center with
        points = 90;
        last_attack_time = Unix.gettimeofday ();
      };
    model_follow_tests "Follow on empty_world_server" empty_world_server
      non_moving_entity_at_origin non_moving_entity_at_origin;
    model_follow_tests
      "Follow on world_1_server with non_moving_entity_at_origin"
      world_1_server non_moving_entity_at_origin
      non_moving_entity_at_origin;
    model_follow_tests
      "Follow on world_1_server with non_moving_camel_at_origin"
      world_1_server non_moving_camel_at_origin
      non_moving_camel_at_origin;
    model_follow_tests
      "Follow on world_1_server with moving_camel_near_entity_3"
      world_1_server moving_camel_near_entity_3
      { moving_camel_near_entity_3 with vx = 0.; vy = 0. };
    model_try_acquire_imprint_tests
      "Try to acquire imprint on empty_world_server and entity = \
       non_moving_camel_at_origin (a camel)"
      empty_world_server non_moving_camel_at_origin
      non_moving_camel_at_origin;
    model_try_acquire_imprint_tests
      "Try to acquire imprint on world_1_server and entity = \
       non_moving_camel_at_origin (a camel)"
      world_1_server non_moving_camel_at_origin
      non_moving_camel_at_origin;
    model_try_acquire_imprint_tests
      "Try to acquire imprint on world_1_server and entity =  \
       moving_camel_near_entity_3"
      world_1_server moving_camel_near_entity_3
      moving_camel_near_entity_3;
    model_apply_enemy_step_tests
      "Apply enemey step on world_1_server with \
       non_moving_camel_at_origin "
      world_1_server non_moving_camel_at_origin
      {
        non_moving_camel_at_origin with
        last_attack_time = Unix.gettimeofday ();
      };
    model_apply_enemy_step_tests
      "Apply enemey step on world_1_server with moving_entity_at_origin"
      world_1_server moving_entity_at_origin
      {
        moving_entity_at_origin with
        vx = 0.;
        vy = 0.;
        last_attack_time = Unix.gettimeofday ();
      };
    model_apply_enemy_step_tests
      "Apply enemey step on world_1_server with entity_3" world_1_server
      entity_3
      {
        entity_3 with
        vx = 37.4437999357;
        vy = -33.6994199422;
        last_attack_time = Unix.gettimeofday ();
      };
    model_apply_enemy_step_tests
      "Apply enemey step on world_1_server with \
       moving_camel_near_entity_3"
      world_1_server moving_camel_near_entity_3
      {
        moving_camel_near_entity_3 with
        vx = 37.4437999357;
        vy = -33.6994199422;
        last_attack_time = Unix.gettimeofday ();
      };
    model_apply_enemy_step_tests
      "Apply enemey step on world_4_server with \
       moving_camel_near_entity_3"
      world_4_server entity_5_center
      {
        entity_5_center with
        last_attack_time = Unix.gettimeofday ();
      };
  ]

let suite =
  "test suite for Clash of Camels"
  >::: List.flatten [ world_manager_tests; model_tests ]

let _ = run_test_tt_main suite
