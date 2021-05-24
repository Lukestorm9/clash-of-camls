(** [try_sock_connect addr port] tries to open a connection to a socket
    listening at addr:port. Returns Some(sock) if successful, None
    otherwise. *)
let try_sock_connect addr port =
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  let sock_addr =
    Unix.ADDR_INET (Unix.inet_addr_of_string addr, port)
  in
  try
    Unix.connect sock sock_addr;
    Some sock
  with _ -> None

(** [nowify e] updates the time sent over of an entity option [e] to the
    current Unix time if that entity option is Some e*)
let nowify (e : Common.entity option) =
  match e with
  | None -> None
  | Some e ->
      let now = Unix.gettimeofday () in
      Some { e with time_sent_over = now }

(** [adjust_last_attack_time delta e] optionally adjusts the last attack
    time of an entity. This is to adjust for wierd unix time issues of
    different computers. *)
let adjust_last_attack_time delta (e : Common.entity option) =
  match e with
  | None -> None
  | Some e ->
      Some { e with last_attack_time = e.last_attack_time +. delta }

let sanitize_action
    (action : Common.action)
    (last_action : Common.action ref) =
  match (action, !last_action) with
  | Buy _, Buy _ -> Common.Nothing
  | _ ->
      last_action := action;
      action

(** [client_loop sock_state_pair] is the client update loop. It pulls
    data from the server, and sticks it in the shared mutable state. *)
let client_loop ((sock, state) : Unix.file_descr * Common.world_state) =
  let recv_chan = Unix.in_channel_of_descr sock in
  let send_chan = Unix.out_channel_of_descr sock in
  Marshal.to_channel send_chan 1776 [];
  flush send_chan;

  (* Read an assign the uuid corresponding to the player from the server *)
  let uuid = (Marshal.from_channel recv_chan : int) in
  "Successfully logged in w/ uuid = " ^ string_of_int uuid
  |> print_endline;
  Mutex.lock state.mutex;
  state.uuid := Some uuid;
  Mutex.unlock state.mutex;

  (* Listen to updates *)
  let last_action = ref Common.Nothing in
  try
    while true do
      (* State receive step *)
      let (won_opt, time, noveau) =
        ( Marshal.from_channel recv_chan
          : int option * float * Common.entity option array )
      in
      let noveau = Array.map nowify noveau in
      let delta = Unix.gettimeofday () -. time in
      let noveau = Array.map (adjust_last_attack_time delta) noveau in

      let len = Array.length noveau in

      (* State update step *)
      Mutex.lock state.mutex;
      state.winner := won_opt;
      Array.blit noveau 0 state.data 0 len;
      let action = sanitize_action !(state.user_command) last_action in
      let pair = (Option.get !(state.uuid), action) in
      Mutex.unlock state.mutex;

      (* User action transmit step *)
      Marshal.to_channel send_chan pair [];
      flush send_chan
    done
  with _ -> print_endline "Connection to server lost"

(** [start addr port] initializes the shared state, and then spins up
    the new thread for the client to be on. Attempts to connect a bunch
    of times to deal with a wierd connection bug. *)
let start addr port =
  let state : Common.world_state =
    {
      data = Array.make 500 None;
      mutex = Mutex.create ();
      uuid = ref None;
      winner = ref None;
      user_command = ref Common.Nothing;
    }
  in
  let rec attempt_connect i =
    if i = 0 then None
    else
      match try_sock_connect addr port with
      | Some sock ->
          Some (state, Thread.create client_loop (sock, state))
      | None -> attempt_connect (i - 1)
  in
  attempt_connect 10
