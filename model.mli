val process_movement :
  Common.entity -> Common.direction -> Common.entity

(**[get_local_enemies state entity radius direction] computes the
   enemies possible for attack given a world state, entity, radius, and
   direction of attack. A list of local enemies and their coressponding
   index value in the world state array will be returned. Note that
   local enemy is defined as being a distance equal to the radius or
   less away from the given entity and within the direction given. Each
   direction has an acceptable range the enemy could be in.

   Directions: Right -> Pi/4 to -Pi/4 ||| Left -> 3Pi/4 to -3Pi/4 ||| Up
   \-> 3Pi/4 to Pi/4 ||| Down -> -3Pi/4 to -Pi/4 *)
val get_local_enemies :
  Common.serv_state ->
  Common.entity ->
  float ->
  Common.direction ->
  (int * Common.entity) list

val process_attack :
  Common.serv_state ->
  Common.entity ->
  Common.direction ->
  Common.entity

val follow : Common.serv_state -> Common.entity -> Common.entity

val try_acquire_imprint :
  Common.serv_state -> Common.entity -> Common.entity

val apply_enemy_step :
  Common.serv_state -> Common.entity -> Common.entity
