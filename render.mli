(** This module renders everything on screen. *)

(** [run world map] takes in world state [world] and hashmap [map] and
    renders the world map and every entity in world along with its
    health, score, and inventory if its a player and its health if its
    not a player with the associated graphics in the hashmap. *)
val run :
  Common.world_state -> (string, Sdlvideo.surface) Hashtbl.t -> unit
