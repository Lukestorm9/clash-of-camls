(** This this section outlines the test plan.

    1. Parts of the system were automatically tested by OUnit vs.
    manually tested: We test the [World Manager] functions using OUnit
    testing. [Render], [Client], and parts of [Client] are te test
    manually as there is good way to test the functions those modules.
    [World Manager] has functions that well-suited for testing using the
    OUnit suite, thus it was not manually tested.

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
open Server

let print_entity_type (kind : Common.entity_type) =
  if kind = Physik then "Physik"
  else if kind = Ai then "Ai"
  else "Player"

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

(*[print_entity] prints the given entity_list. An entity is defined in
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

(*[compare_indexes] compares that the indexes outputed are same
  regarless of the order they are in expected or calculated. The sorting
  process does not remove duplicate values. *)
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

(*[print_float_pair_option] prints and float option pair. If the option
  pair is some then it prints the float pair else if None then this
  function prints the word "None"*)
let print_float_pair_option (pair : (float * float) option) =
  match pair with
  | Some (s, t) ->
      "( " ^ string_of_float s ^ ", " ^ string_of_float t ^ " )"
  | None -> "None"

let rec check_within_bounds_helper
    (expected_world : Common.entity list)
    (calculated_world : Common.entity list)
    (boundary : float) : bool =
  match (expected_world, calculated_world) with
  | [], [] -> true
  | exp :: t, cal :: m ->
      if
        cal.x +. boundary >= exp.x
        && cal.x -. boundary <= exp.x
        && cal.y +. boundary >= exp.y
        && cal.y -. boundary <= exp.y
      then check_within_bounds_helper t m boundary
      else false
  | _ :: _, [] | [], _ :: _ -> false

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

let check_within_bounds_float_pair
    (float_pair_expected : (float * float) option)
    (float_pair_given : (float * float) option) : bool =
  let boundary = 0.5 in
  check_within_bounds_float_pair_helper float_pair_expected
    float_pair_given boundary

(*[check_within_bounds] checks if the calculated_world's
  (calculated_word is the original world passed to
  World_manager.get_local) x,y pair is within a bounded number with
  expected_world's x,y pair, and the functions does for all entites. The
  bounds can be changed by changing bounded. Currently the bounded is
  set to 0.5
  *******************************************************************************
  Ex with only one entity: expect_world's x,y: (0, 0) output_world's
  x,y: (1, 0.5) -> check_within_bounds produces false because
  output_world's 1 is out of bounds given a bounds of 0.5. The
  acceptable bounds for x,y for -0.5 and +0.5 for this example.*)
let rec check_within_bounds
    (expected_world : Common.entity list)
    (calculated_world : Common.entity list) : bool =
  let bounded = 0.5 in
  check_within_bounds_helper expected_world calculated_world bounded

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
    ~cmp:check_within_bounds ~printer:(print_entity_list "")

let world_manager_get_player_xy_tests
    (name : string)
    (state : Common.world_state)
    (expected_output : (float * float) option) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (World_manager.get_player_xy state)
    ~cmp:check_within_bounds_float_pair ~printer:print_float_pair_option

let server_get_local_enemies_tests
    (name : string)
    state
    (entity : Common.entity)
    radius
    direction
    (expected_output : (int * Common.entity) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Server.get_local_enemies state entity radius direction)
    ~cmp:compare_indexes
    ~printer:(print_int_entity_list "")

let world_state_maker ~data ~mutex ~uuid ~user_command :
    Common.world_state =
  { data; mutex; uuid; user_command }

let world_state_maker_server ~data ~points_gathered ~mutex :
    Server.world_state =
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

let non_moving_entity_at_origin =
  entity_maker Common.Player 1 0. 0. 0. 0. 0. "camel" 0. false [] 0 0.

let moving_entity_at_origin =
  entity_maker Common.Player 2 0. 0. 5. 5. 1. "camel" 0. false [] 0 0.

let moving_entity_at_origin' =
  entity_maker Common.Player 0
    (0. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    (0. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    5. 5. 1. "camel" 0. false [] 0 0.

let entity_3 =
  entity_maker Common.Player 3 (-100.) 90. 5. 5. 1. "camel" 0. false []
    0 0.

let entity_3' =
  entity_maker Common.Player 3
    (-100. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    (90. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    5. 5. 1. "camel" 0. false [] 0 0.

let entity_4 =
  entity_maker Common.Player 4 0. 3. 5. 5. 1. "camel" 0. true [] 0 0.

let entity_5_center =
  entity_maker Common.Player 5 10. (-2.) 5. 5. 1. "camel" 0. true [] 0
    0.

let entity_6_down =
  entity_maker Common.Player 6 12. (-5.) 5. 5. 1. "camel" 0. true [] 0
    0.

let entity_6_down' =
  entity_maker Common.Player 6 (-5.) (-20.) 5. 5. 1. "camel" 0. true []
    0 0.

let entity_7_right =
  entity_maker Common.Player 7 40. (-10.) 5. 5. 1. "camel" 0. true [] 0
    0.

let entity_7_right' =
  entity_maker Common.Player 7 60. 20. 5. 5. 1. "camel" 0. true [] 0 0.

let entity_8_up =
  entity_maker Common.Player 8 8. 4. 5. 5. 1. "camel" 0. true [] 0 0.

let entity_8_up' =
  entity_maker Common.Player 8 (-20.) 30. 5. 5. 1. "camel" 0. true [] 0
    0.

let entity_9_left =
  entity_maker Common.Player 9 (-11.) (-10.) 5. 5. 1. "camel" 0. true []
    0 0.

let entity_9_left' =
  entity_maker Common.Player 9 5. 1. 5. 5. 1. "camel" 0. true [] 0 0.

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
    [| Some non_moving_entity_at_origin; Some entity_3 |]
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

let server_tests =
  [
    server_get_local_enemies_tests
      "Trying to find an enemy on an empty with radius 1."
      empty_world_server non_moving_entity_at_origin 1. Up [];
    server_get_local_enemies_tests
      "Trying to find an enemy on world_2_server | r= 0, entity= \
       non_moving-entity_at_orgin"
      world_2_server non_moving_entity_at_origin 0. Up [];
    server_get_local_enemies_tests
      "Trying to find an enemy on world_2_server | r= 100.0, entity= \
       entity_3"
      world_2_server entity_3 100.0 Up [];
    server_get_local_enemies_tests
      "Trying to find enemies on world_2_server | r=boundary_point_2 - \
       0.001, entity= entity_3"
      world_2_server entity_3
      (boundary_point_2 -. 0.001)
      Up [];
    (*Testing that Left works*)
    server_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Up"
      world_1_server non_moving_entity_at_origin boundary_point_2 Up [];
    server_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Right"
      world_1_server non_moving_entity_at_origin boundary_point_2 Down
      [];
    server_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Down"
      world_1_server non_moving_entity_at_origin boundary_point_2 Right
      [];
    server_get_local_enemies_tests
      "Trying to find enemies on world_1_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Left"
      world_1_server non_moving_entity_at_origin boundary_point_2 Left
      [ (1, entity_3) ];
    (*Testing that Up works*)
    server_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Up"
      world_3_server non_moving_entity_at_origin boundary_point_2 Up
      [ (2, entity_4) ];
    server_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Down"
      world_3_server non_moving_entity_at_origin boundary_point_2 Down
      [];
    server_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Right"
      world_3_server non_moving_entity_at_origin boundary_point_2 Right
      [];
    server_get_local_enemies_tests
      "Trying to find enemies on world_3_server | r=boundary_point_2, \
       entity= non_moving_entity_at_origin, d = Left"
      world_3_server non_moving_entity_at_origin boundary_point_2 Left
      [ (1, entity_3) ];
    (*Testing get_local_enemies with center not origin*)
    (*Testing that Up works*)
    server_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Up"
      world_4_server entity_5_center boundary_point_2 Up
      [ (7, entity_8_up'); (3, entity_8_up) ];
    server_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Down"
      world_4_server entity_5_center boundary_point_2 Down
      [ (5, entity_6_down'); (1, entity_6_down) ];
    server_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Down"
      world_4_server entity_5_center boundary_point_2 Right
      [ (6, entity_7_right'); (2, entity_7_right) ];
    server_get_local_enemies_tests
      "Trying to find enemies on world_4_server | r=boundary_point_2, \
       entity= entity_5_center, d = Down"
      world_4_server entity_5_center boundary_point_2 Left
      [ (8, entity_9_left'); (4, entity_9_left) ];
  ]

let suite =
  "test suite for Clash of Camels"
  >::: List.flatten [ world_manager_tests; server_tests ]

let _ = run_test_tt_main suite
