type weapon = {
  name : string;
  range : float;
  damage : float;
  cooldown : float;
}

type enemy = {
  name : string;
  health : float;
  graphic : string;
  points : int;
  weapon : weapon;
}

val load_enemies : unit -> enemy list

val load_weapons : unit -> weapon list
