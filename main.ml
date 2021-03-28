let create_hash () =
  let image_hash = Hashtbl.create 10 in
  let files = Sys.readdir "assets" in
  let images =
    Array.map (fun x -> "assets/" ^ x |> Sdlloader.load_image) files
  in
  Array.map2 (fun a b -> Hashtbl.add image_hash a b) files images
  |> ignore;
  image_hash

(* 1. make play CMD "" for start: start clients -> return world state
   that is working -if Option = None -> failwith "Not valid server
   address" else do what is specified 2. client: supply port *)

(**[parser_client] parses command line arguments and executes the right
   commands. RI: The first argument must be CMD. The second argument
   must be a string with no extra spaces, that is say only one space
   between each argument and no spaces before or after the second string
   argument. The string must not have a length greater than 2.*)

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
