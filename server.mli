(** [start port] starts a server locally at the specified [port]. It
    returns handles to a pair of threads. The first thread is the
    physics thread. The second thread is the networking thread. It is
    safe to ignore these, they are simply provided in case we want to do
    something with them in the future. (i.e. join)*)
val start : int -> Thread.t * Thread.t

(* A server-specific world state. It tracks information that is strictly
   different from the information tracked by the client, including UUID
   metadata, and does not track client-specific information, such as the
   UUID associated with a particular client. *)
type world_state = {
  data : Common.entity option array;
  mutex : Mutex.t;
}

(**[get_local_enemies] computes the enemies possible for attack given a
   world state, entity, radius, and direction of attack. A list of local
   enemies and their coressponding index value in the world state array
   will be returned. Note that local enemy is defined as being a
   distance equal to the radius or less away from the given entity and
   within the direction given. Each direction has an acceptable range
   the enemy could be in.
   **********************************************************************
   Direction: Right -> π/4 to -π/4 ||| Left -> 3π/4 to -3π/4 ||| Up
   \-> 3π/4 to π/4 ||| Down -> -3π/4 to -π/4 *)
val get_local_enemies :
  world_state ->
  Common.entity ->
  float ->
  Common.direction ->
  (int * Common.entity) list
