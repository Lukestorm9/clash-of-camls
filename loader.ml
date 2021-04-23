(**Enemey.json: parse a list enemey list Enemy: -Health -Graphic -Points
   value -Weapon_name: weapon Weapons.json: parse as a list of weapons

   parse_weapon: use for weapon_name & weapons.json *)
open Yojson.Basic.Util
type weapon = {
  name : string;
  range : float;
  damage : float;
  cooldown : float;
}

type enemy = {
  name: string; 
  health : float;
  graphic : string;
  points : int;
  weapon : weapon;
}

let weapon_of_json w =
  {
    name = w |> member "name" |> to_string;
    range = w |> member "range" |> to_float;
    damage = w |> member "damage" |> to_float;
    cooldown = w |> member "cooldown" |> to_float;
  }

let enemy_of_json e =
  {
    name  = e |> member "name" |> to_string;
    health = e |> member "health" |> to_float;
    graphic = e |> member "graphic" |> to_string;
    points = e |> member "points" |> to_int;
    weapon = e |> member "weapon" |> weapon_of_json;
  }

let parse_enemy e =
  try enemy_of_json e
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let parse_weapon w =
  try weapon_of_json w
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let load_enemies () =
  let e = Yojson.Basic.from_file "enemies.json" in
  e |> member "data" |> to_list |> List.map enemy_of_json

let load_weapons () =
  let w = Yojson.Basic.from_file "weapon.json" in
  w |> member "data" |> to_list |> List.map weapon_of_json
