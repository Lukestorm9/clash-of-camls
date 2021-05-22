(** This module is responsible for the loading of any serverside
    definitions. This includes weapon types, enemies, etc. *)

(** A prototype for an enemy, and for this reason NOT equivalent to a
    common entity *)
type enemy = {
  name : string;
  health : float;
  graphic : string;
  points : int;
  weapon : Common.weapon;
}

(**[load_enemies] loads enemy types from enemies.json*)
val load_enemies : unit -> enemy list

(**[load_enemies] loads enemy types from weapons.json*)
val load_weapons : unit -> Common.weapon list
