(** Contains a lot of common definitions. These type definitions are
    exposed in common.mli so that other files, which need to access
    these internals can do so. In order for the code to compile, they
    have to be repeated in common.ml, otherwise we would not give them
    there.*)

(** The type of the entity *)
type entity_type =
  | Physik
  | Ai
  | Player
  | Camel of int option
  | Merchant

(** A weapon type *)
type weapon = {
  name : string;
  range : float;
  damage : float;
  cooldown : float;
}

(** [entity] represents an entity, as it is represented on both the
    client and the server. This dualism is important, as otherwise
    Marshall-ing would not be so easy.*)
type entity = {
  (*whether or not this is an AI controlled entity *)
  kind : entity_type;
  (* The universally unique id associated with this entity. Guaranteed
     never to change over the lifecycle of the entity.*)
  uuid : int;
  (* The x position of the entity *)
  x : float;
  (* The y position of the entity *)
  y : float;
  (* The x velocity of the entity *)
  vx : float;
  (* The y velocity of the entity *)
  vy : float;
  (* The time at which the server sent over the entity to the client.*)
  time_sent_over : float;
  (* The graphic of the entity *)
  graphic : string;
  (* The health of the entity *)
  health : float;
  (* The max health this kind of entity can heal to*)
  max_health : float;
  (* The last direction moved -- in particular, true iff right*)
  last_direction_moved : bool;
  (*The inventory associated with this entity*)
  inventory : weapon list;
  (*The points of the entity*)
  points : int;
  (* last time entity did attack *)
  last_attack_time : float;
}

(** A general compass direction type*)
type direction =
  | Left
  | Right
  | Up
  | Down

(** The types of actions that a player may take *)
type action =
  | Nothing
  | Move of direction
  | Attack of direction

(** [world_state] represents the world state as the client, NOT the
    server, understands it. Before accessing any of the data-bearing
    fields, it is imperative that the Mutex be locked. It is equally
    imperative that the mutex be unlocked afterwards. Otherwise,
    multithreading bad-ness occurs. You have been warned. *)
type world_state = {
  (* A collection of entities that the client knows about. There are no
     guarantees about the order of entities with respect to their uuids. *)
  data : entity option array;
  (* A uuid representing the uuid of the entity corresponding to the
     player character. If None, that has not yet logged in fully, or is
     spectating. It is NOT guaranteed that an entity with this uuid will
     exist in the data array, for example in the case where the player
     character has perished. *)
  uuid : int option ref;
  (* The last thing a user wanted to do*)
  user_command : action ref;
  (* The mutex for the world state. The structure requires this to be
     locked before accessing any of its fields. *)
  mutex : Mutex.t;
}

(** [serv_state] represents the state of the game as the server
    understands it. It is in Common for the purpose of convincing
    ocamlbuild to compile the program, and not for philsophical reasons. *)
type serv_state = {
  (* The same as the data of the world_state *)
  data : entity option array;
  (* The total amount of points gathered by all players *)
  points_gathered : int ref;
  (* A mutex to lock for any multithreaded operations on the state *)
  mutex : Mutex.t;
}

(** [array_filter pred arr] finds the entities that satisfy a predicate
    and returns a list of those entities. Requires that that the array
    not change during the optionation, i.e. that any mutex for the
    world_state be held at the time that this function is called. Note
    that OCAML's array does not provide a filter operation by default:
    https://ocaml.org/api/Array.html *)
val array_filter : ('a -> bool) -> 'a option array -> 'a list

(** [array_index_of pred array] finds the first present element in an
    optional array that marches a predicate. While superficially similar
    to List find this function by necessity returns an index for the
    purpose of state manipulation. Note that OCAML's arrays and lists do
    not provide equivalent functionality by default:
    https://ocaml.org/api/Array.html https://ocaml.org/api/List.html . *)
val array_index_of : ('a -> bool) -> 'a option array -> int option
