open OUnit2
open World_manager

(*[print_entity] prints the given entity_list. An entity is defined in
  Common.mli*)
let rec print_entity acc (entity_list : Common.entity list) =
  match entity_list with
  | [] -> "[\n" ^ acc ^ " ]"
  | h :: t ->
      let entity =
        "{ " ^ "uuid = " ^ string_of_int h.uuid ^ ", " ^ "x = "
        ^ string_of_float h.x ^ ", " ^ "y = " ^ string_of_float h.y
        ^ ", " ^ "vx = " ^ string_of_float h.vx ^ ", " ^ "vy = "
        ^ string_of_float h.vy ^ ", " ^ "time_sent_over = "
        ^ string_of_float h.time_sent_over
        ^ ", " ^ "graphic = " ^ h.graphic ^ ", " ^ "health = "
        ^ string_of_float h.health
        ^ " }" ^ ";\n"
      in
      print_entity (entity ^ acc) t

(*[print_float_pair_optio] prints and float option pair. If the option
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
    ~cmp:check_within_bounds ~printer:(print_entity "")

let world_manager_get_player_xy_tests
    (name : string)
    (state : Common.world_state)
    (expected_output : (float * float) option) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (World_manager.get_player_xy state)
    ~cmp:check_within_bounds_float_pair ~printer:print_float_pair_option

let world_state_maker ~data ~mutex ~uuid ~user_command :
    Common.world_state =
  { data; mutex; uuid; user_command }

let entity_maker
    ~uuid
    ~x
    ~y
    ~vx
    ~vy
    ~time_sent_over
    ~graphic
    ~health
    ~last_direction_moved : Common.entity =
  {
    uuid;
    x;
    y;
    vx;
    vy;
    time_sent_over;
    graphic;
    health;
    last_direction_moved;
  }

let (non_moving_entity_at_origin : Common.entity) =
  entity_maker 1 0. 0. 0. 0. 0. "camel" 0. false

let (moving_entity_at_origin : Common.entity) =
  entity_maker 2 0. 0. 5. 5. 1. "camel" 0. false

let (moving_entity_at_origin' : Common.entity) =
  entity_maker 0
    (0. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    (0. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    5. 5. 1. "camel" 0. false

let (entity_3 : Common.entity) =
  entity_maker 3 (-100.) 90. 5. 5. 1. "camel" 0. false

let (entity_3' : Common.entity) =
  entity_maker 3
    (-100. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    (90. +. (5. *. (Unix.gettimeofday () +. (1. /. 4.) -. 1.)))
    5. 5. 1. "camel" 0. false

let (empty_world : Common.world_state) =
  world_state_maker [||] (Mutex.create ()) (ref (Some 0))
    (ref Common.Nothing)

let (world_alpha : Common.world_state) =
  world_state_maker [||] (Mutex.create ()) (ref (Some 0))
    (ref Common.Nothing)

let (world_0 : Common.world_state) =
  world_state_maker
    [| Some non_moving_entity_at_origin |]
    (Mutex.create ()) (ref (Some 0)) (ref Common.Nothing)

let (world_1 : Common.world_state) =
  world_state_maker
    [| Some non_moving_entity_at_origin; Some moving_entity_at_origin |]
    (Mutex.create ()) (ref (Some 1)) (ref Common.Nothing)

let (world_2 : Common.world_state) =
  world_state_maker
    [|
      Some non_moving_entity_at_origin;
      Some moving_entity_at_origin;
      Some entity_3;
    |]
    (Mutex.create ()) (ref (Some 3)) (ref Common.Nothing)

let (world_3 : Common.world_state) =
  world_state_maker
    [|
      Some non_moving_entity_at_origin;
      Some moving_entity_at_origin;
      Some entity_3;
    |]
    (Mutex.create ()) (ref (Some 4)) (ref Common.Nothing)

let boundary_point = sqrt ((250000. ** 2.) /. 2.)

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

let suite =
  "test suite for Clash of Camels"
  >::: List.flatten [ world_manager_tests ]

let _ = run_test_tt_main suite
