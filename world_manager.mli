(**[get_local] find local entities x,y position in a radius of 250,000
   pixel and applies [location_smoothing]. [location_smoothing] takes
   the calculated "local" entities and predicts where such entities will be*)
val get_local :
  Common.world_state -> float -> float -> Common.entity list
