(* copy of the definition in common.mli *)
type entity = {
  uuid : int;
  x : float;
  y : float;
  vx : float;
  vy : float;
  time_sent_over : float;
  graphic : string;
  health : float;
}

type action =
  | Nothing
  | Left
  | Right
  | Up
  | Down

(* copy of the definition in common.mli *)
type world_state = {
  data : entity option array;
  uuid : int option ref;
  user_command : action ref;
  mutex : Mutex.t;
}
