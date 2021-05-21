(* These type definitions are exposed in common.mli so that other files,
   which need to access these internals can do so. In order for the code
   to compile, they have to be repeated in common.ml, otherwise we would
   not give them there.*)

(* The type of the entity *)
type entity_type =
  | Physik
  | Ai
  | Player
  | Camel of int option
  | Merchant

(* A weapon type *)
type weapon = {
  name : string;
  range : float;
  damage : float;
  cooldown : float;
}

(* [entity] represents an entity, as it is represented on both the
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

type direction =
  | Left
  | Right
  | Up
  | Down

type action =
  | Nothing
  | Move of direction
  | Attack of direction

(* [world_state] represents the world state as the client, NOT the
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

type serv_state = {
  data : entity option array;
  points_gathered : int ref;
  mutex : Mutex.t;
}

val array_filter : ('a -> bool) -> 'a option array -> 'a list

val array_index_of : ('a -> bool) -> 'a option array -> int option
