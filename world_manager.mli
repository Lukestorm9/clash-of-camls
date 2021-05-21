(**[get_local] find local entities x,y position in a radius of 250,000
   pixel and applies location smoothing. Location smoothing takes the
   calculated "local" entities and predicts where such entities will be
   some time after it was received*)
val get_local :
  Common.world_state -> float -> float -> Common.entity list

(**[get_player_xy] returns a player id if World_state has a uuid. If
   such a uuid exists search world_state.data for the uuid and return (x
   * y) is exists else None*)
val get_player_xy : Common.world_state -> (float * float) option

(**[get_player_uuid] gets the player uuid*)
val get_player_uuid : Common.world_state -> int option
