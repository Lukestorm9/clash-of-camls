(**[get_local] find local entities x,y position in a radius of 250,000
   pixel and applies [location_smoothing]. [location_smoothing] takes
   the calculated "local" entities and predicts where such entities will
   be*)
val get_local :
  Common.world_state -> float -> float -> Common.entity list

(***[get_player_xy] returns a player id if World_state has a uuid. If
  such a uuid exists search world_state.data for the uuid and return (x
  * y) is exists else None*)
val get_player_xy : Common.world_state -> (float * float) option

(**[get_local_enemies] returns a list of all other than itself that are
   within the specified radius, and facing the same direction as the
   given entity. Note enemies on the boundary (that is enemies that
   exactly a distance equivalent to r) will be counted in the enemy
   list.*)
val get_local_enemies :
  Common.world_state -> Common.entity -> float -> Common.entity list
