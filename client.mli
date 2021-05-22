(** This module is responsible for starting a client connection to a
    possibly remote server. Local servers are handled over localhost. *)

(** [start addr port] is Some(state, thread) if addr and port point to a
    server listening at addr:port. It is None if the connection fails.

    Thus, if successful, it creates a client listener, which reads data
    from the server, and the updates the world state from its own thread
    whenever it reads new information.

    It returns a reference to a bit of shared mutable state, which the
    client guarantees to lock the mutex of, then write new information
    from the server to, and then unlock the mutex of. It is imperative
    that other code reading from the world state refrain from
    reading/writing to the world state without locking/unlocking the
    Mutex.

    Also returns a handle to the thread which the client is running on,
    if that is useful. This can be safely ignored. *)
val start : string -> int -> (Common.world_state * Thread.t) option
