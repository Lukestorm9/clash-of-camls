let create_hash () =
  let image_hash = Hashtbl.create 10 in
  let files = Sys.readdir "assets" in
  let images =
    Array.map (fun x -> "assets/" ^ x |> Sdlloader.load_image) files
  in
  Array.map2 (fun a b -> Hashtbl.add image_hash a b) files images
  |> ignore;
  image_hash

(*[remote_config] configures remote connection for the client. It checks
  if the number of arguments are correct.*)
let remote_config number_of_args arguments =
  try
    if number_of_args != 4 then
      print_endline
        "Malformed argument: Needs only four arguments for remote"
    else if int_of_string arguments.(3) < 0 then
      print_endline "Malformed argument: Port should be greater than 0"
    else
      let port = int_of_string arguments.(3) in
      let client = Client.start arguments.(2) port in
      match client with
      | Some (world_state, thread) ->
          Sdl.init [ `VIDEO ];
          create_hash () |> Render.run world_state
      | None -> print_endline "Not valid server address"
  with Failure _ -> print_endline "Unable to parse input"

(*[local_config] configures local connection for the client. Client will
  start at "0.0.0.0". You must provide a port. It checks if the number
  of arguments are correct.*)
let local_config number_of_args arguments =
  try
    if number_of_args != 3 then
      print_endline
        "Malformed argument: Needs only three arguments for local"
    else if int_of_string arguments.(2) < 0 then
      print_endline "Malformed argument"
    else
      let port = int_of_string arguments.(2) in
      Server.start port |> ignore;
      let client = Client.start "0.0.0.0" port in
      match client with
      | Some (world_state, thread) ->
          Sdl.init [ `VIDEO ];
          create_hash () |> Render.run world_state
      | None -> print_endline "Not valid server address"
  with Failure _ -> print_endline "Unable to parse input"

(*[main] parses command line arguments and executes the right commands.
  You can make either a local configuration or remote configuration. The
  local configuration is for single player, and the remote configuration
  is multi-player. RI: The first argument must be CMD. The second
  argument must be a string with no extra spaces, that is say only one
  space between each argument and no spaces before or after the second
  string argument. The string must not have a length greater than 2.
  Examples of commands one could executed: (1) make play CMD="local
  <port>" (2) make play CMD="remote <client> <port>"*)
let main () =
  let number_of_args = Array.length Sys.argv in
  match Sys.argv.(1) with
  | "remote" -> remote_config number_of_args Sys.argv
  | "local" -> local_config number_of_args Sys.argv
  | _ ->
      print_endline
        "Malformed arugment; first argument is neither 'local' or \
         'remote'"

let _ = main ()
