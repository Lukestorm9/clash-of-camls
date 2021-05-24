(** There are some copies of the type definition in common.mli. These
    are necessary public in order to allow other files to work with them
    directly. In order to convince ocamlbuild to compile, they have to
    be replicated here. *)
type entity_type =
  | Physik
  | Ai
  | Player
  | Camel of int option
  | Merchant
  | Trailer

type weapon = {
  name : string;
  range : float;
  damage : float;
  cooldown : float;
}

type entity = {
  kind : entity_type;
  uuid : int;
  x : float;
  y : float;
  vx : float;
  vy : float;
  time_sent_over : float;
  graphic : string;
  health : float;
  max_health : float;
  last_direction_moved : bool;
  inventory : weapon list;
  points : int;
  last_attack_time : float;
}

type direction =
  | Left
  | Right
  | Up
  | Down

type action =
  | Nothing
  | Move of direction
  | Attack of direction
  | Buy of int

type world_state = {
  data : entity option array;
  uuid : int option ref;
  user_command : action ref;
  winner : int option ref;
  mutex : Mutex.t;
}

type serv_state = {
  data : entity option array;
  points_gathered : int ref;
  mutex : Mutex.t;
}

(** [array_filter pred arr] finds the entities that satisfy a predicate
    and returns a list of those entities. Requires that that the array
    not change during the optionation, i.e. that any mutex for the
    world_state be held at the time that this function is called. Note
    that OCAML's array does not provide a filter operation by default:
    https://ocaml.org/api/Array.html *)
let array_filter pred arr =
  Array.to_list arr
  |> List.filter_map Stdlib.Fun.id
  |> List.filter pred |> List.rev

(** [array_index_of pred array] finds the first present element in an
    optional array that marches a predicate. While superficially similar
    to List find this function by necessity returns an index for the
    purpose of state manipulation. Note that OCAML's arrays and lists do
    not provide equivalent functionality by default:
    https://ocaml.org/api/Array.html https://ocaml.org/api/List.html . *)
let array_index_of pred arr =
  let idex = ref None in
  Array.iteri
    (fun i e ->
      match e with
      | Some e -> if pred e then idex := Some i else ()
      | None -> ())
    arr;
  !idex
