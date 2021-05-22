(** *)

(** A prototype for an enemy, and for this reason NOT equivalent to a
    common entity *)
type enemy = {
  name : string;
  health : float;
  graphic : string;
  points : int;
  weapon : Common.weapon;
}

val load_enemies : unit -> enemy list

val load_weapons : unit -> Common.weapon list
