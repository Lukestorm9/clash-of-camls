type weapon = {
  name : string;
  range : float;
  damage : float;
  cooldown : float;
}

type enemy = {
  health : float;
  graphic : string;
  points : int;
  weapon_name : weapon;
}

val weapon_of_json: Yojson.Basic.t -> weapon 

val enemy_of_json: Yojson.Basic.t -> enemy 

val parse_enemy: Yojson.Basic.t -> enemy

val parse_weapon: Yojson.Basic.t -> weapon

val load_enemies: unit -> enemy list 

val load_weapons: unit -> weapon list 
