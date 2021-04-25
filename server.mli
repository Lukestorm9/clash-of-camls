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
  highest_uuid : int ref;
}

val get_local_enemies :
  world_state ->
  Common.entity ->
  float ->
  Common.direction ->
  (int * Common.entity) list
