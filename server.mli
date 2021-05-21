(** This module represents the game server *)

(** [start port] starts a server locally at the specified [port]. It
    returns handles to a pair of threads. The first thread is the
    physics thread. The second thread is the networking thread. It is
    safe to ignore these, they are simply provided in case we want to do
    something with them in the future (i.e. join on program
    termination). *)
val start : int -> Thread.t * Thread.t
