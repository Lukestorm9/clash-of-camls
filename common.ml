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
  last_direction_moved : bool;
  inventory: string list; 
  points: int;
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

(*[array_filter] finds the entities that satisfy a predicate and returns
  a list of those entities. Requires that the for the world_state be
  held at the time that this function is called. *)
let array_filter pred arr =
  Array.fold_left
    (fun acc t ->
      match t with
      | Some t -> if pred t then t :: acc else acc
      | None -> acc)
    [] arr

let array_index_of pred arr =
  let idex = ref None in
  Array.iteri
    (fun i e ->
      match e with
      | Some e -> if pred e then idex := Some i else ()
      | None -> ())
    arr;
  !idex
