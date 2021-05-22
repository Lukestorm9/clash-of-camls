(** This module represents any serverside model calculations. I.e. world
    state updates. This includes AI, attack calculations, etc.*)

(** [get_local_enemies state entity radius direction] computes the
    enemies possible for attack given a world state, entity, radius, and
    direction of attack. A list of local enemies and their coressponding
    index value in the world state array will be returned. Note that
    local enemy is defined as being a distance equal to the radius or
    less away from the given entity and within the direction given. Each
    direction has an acceptable range the enemy could be in.

    Directions: Right -> Pi/4 to -Pi/4 ||| Left -> 3Pi/4 to -3Pi/4 |||
    Up \-> 3Pi/4 to Pi/4 ||| Down -> -3Pi/4 to -Pi/4 *)
val get_local_enemies :
  Common.serv_state ->
  Common.entity ->
  float ->
  Common.direction ->
  (int * Common.entity) list

(** [process_movement e d] updates the velocity of the player into the
    given direction d. Requires that e is in fact the player. *)
val process_movement :
  Common.entity -> Common.direction -> Common.entity

(** [process_attack state entity direction] processes an attack by the
    player in the direction specified. This involves dealing damage to
    all the entities in the arc of the attack. Requires that entity is
    actually a player. *)
val process_attack :
  Common.serv_state ->
  Common.entity ->
  Common.direction ->
  Common.entity

(** [follow state e] updates the velocity of the camel specified by e to
    follow the player that this camel has imprinted on. Requires that e
    is in fact a camel. *)
val follow : Common.serv_state -> Common.entity -> Common.entity

(** [try_acquire_imprint state e] Checks if there is a player within a
    set distance of a camel e. If so, it imprints that camel on the
    player. Requires that e is in fact a camel. *)
val try_acquire_imprint :
  Common.serv_state -> Common.entity -> Common.entity

(** [apply_enemy_step state e] computes AI movement towards the closest
    player for a single enemy at a single time. It also optionally
    attacks that player, if the player is in range, and the time elapsed
    since the last attack is large enough. Requires that e is in fact an
    enemy. *)
val apply_enemy_step :
  Common.serv_state -> Common.entity -> Common.entity
