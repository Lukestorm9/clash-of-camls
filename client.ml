(* Tries to open a connection to a socket listening at addr:port.
   Returns Some(sock) if successful, None otherwise. *)
let try_sock_connect addr port =
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  let sock_addr =
    Unix.ADDR_INET (Unix.inet_addr_of_string addr, port)
  in
  try
    Unix.connect sock sock_addr;
    Some sock
  with _ -> None

(* [nowify e] updates the time sent over of an entity option [e] to the
   current Unix time if that entity option is Some e*)
let nowify (e : Common.entity option) =
  match e with
  | None -> None
  | Some e ->
      let now = Unix.gettimeofday () in
      Some { e with time_sent_over = now }

(* Client update loop. Pulls data from the server, and sticks it in the
   shared mutable state. *)
let client_loop ((sock, state) : Unix.file_descr * Common.world_state) =
  let recv_chan = Unix.in_channel_of_descr sock in
  let send_chan = Unix.out_channel_of_descr sock in
  (* Read an assign the uuid corresponding to the player from the server *)
  let uuid = (Marshal.from_channel recv_chan : int) in
  "Successfully logged in -- was assigned uuid " ^ string_of_int uuid
  |> print_endline;
  Mutex.lock state.mutex;
  state.uuid := Some uuid;
  Mutex.unlock state.mutex;

  (* Listen to updates *)
  try
    while true do
      (* State receive step *)
      let noveau =
        (Marshal.from_channel recv_chan : Common.entity option array)
        |> Array.map nowify
      in
      let len = Array.length noveau in

      (* State update step *)
      Mutex.lock state.mutex;
      Array.blit noveau 0 state.data 0 len;
      let pair = (Option.get !(state.uuid), !(state.user_command)) in
      Mutex.unlock state.mutex;

      (* User action transmit step *)
      Marshal.to_channel send_chan pair [];
      flush send_chan
    done
  with _ -> print_endline "Connection to server lost"

(* This initializes the shared state, and then spins up the new thread
   for the client to be on. *)
let start addr port =
  let state : Common.world_state =
    {
      data = Array.make 500 None;
      mutex = Mutex.create ();
      uuid = ref None;
      user_command = ref Common.Nothing;
    }
  in
  match try_sock_connect addr port with
  | Some sock -> Some (state, Thread.create client_loop (sock, state))
  | None -> None
