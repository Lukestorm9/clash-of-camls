open OUnit2
open World_manager

let rec print_record acc (entity_list : Common.entity list) =
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
      print_record (entity ^ acc) t

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

let rec check_within_bounds
    (expected_world : Common.entity list)
    (calculated_world : Common.entity list) : bool =
  let bounded = 5. in
  check_within_bounds_helper expected_world calculated_world bounded

let world_manager_get_local_tests
    (name : string)
    (state : Common.world_state)
    (x : float)
    (y : float)
    (expected_output : Common.entity list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (World_manager.get_local state x y)
    ~cmp:check_within_bounds ~printer:(print_record "")

let (empty_world : Common.world_state) =
  {
    data = [||];
    mutex = Mutex.create ();
    uuid = ref (Some 0);
    user_command = ref Common.Nothing;
  }

let (non_moving_entity_at_origin : Common.entity) =
  {
    uuid = 0;
    x = 0.;
    y = 0.;
    vx = 0.;
    vy = 0.;
    time_sent_over = 0.;
    graphic = "camel";
    health = 0.;
  }

let (moving_entity_at_origin : Common.entity) =
  {
    uuid = 0;
    x = 0.;
    y = 0.;
    vx = 5.;
    vy = 5.;
    time_sent_over = 1.;
    graphic = "camel";
    health = 0.;
  }

let (moving_entity_at_origin' : Common.entity) =
  {
    uuid = 0;
    x = 5. +. (5. *. (Unix.gettimeofday () -. 1.));
    y = 5. +. (5. *. (Unix.gettimeofday () -. 1.));
    vx = 5.;
    vy = 5.;
    time_sent_over = 1.;
    graphic = "camel";
    health = 0.;
  }

let (world_0 : Common.world_state) =
  {
    data = [| Some non_moving_entity_at_origin |];
    mutex = Mutex.create ();
    uuid = ref (Some 0);
    user_command = ref Common.Nothing;
  }

let (world_1 : Common.world_state) =
  {
    data =
      [|
        Some non_moving_entity_at_origin; Some moving_entity_at_origin;
      |];
    mutex = Mutex.create ();
    uuid = ref (Some 0);
    user_command = ref Common.Nothing;
  }

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
    world_manager_get_local_tests
      "Using world_1 wit (0,0) | Expect all two entities" world_1 0. 0.
      [ non_moving_entity_at_origin; moving_entity_at_origin' ];
  ]

let suite =
  "test suite for Clash of Camels"
  >::: List.flatten [ world_manager_tests ]

let _ = run_test_tt_main suite
