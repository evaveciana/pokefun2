open Pokefun.Pokemon
open Pokefun.Battle

(* Helper function to split a string into chunks of a specified length *)
(* let chunk_string str chunk_size = let length = String.length str in let rec
   aux i acc = if i >= length then List.rev acc else let chunk = if i +
   chunk_size > length then String.sub str i (length - i) else String.sub str i
   chunk_size in aux (i + chunk_size) (chunk :: acc) in aux 0 []

   (* Function to load and parse the CSV file containing ASCII art *) let
   load_ascii_csv filename = let lines = Csv.load filename |> List.concat in let
   rec parse_ascii chunks current acc = function | [] -> List.rev (List.rev
   current :: acc) | line :: rest -> if line = "" then parse_ascii chunks []
   (List.rev current :: acc) rest else parse_ascii chunks (line :: current) acc
   rest in parse_ascii [] [] [] lines

   (* Function to print a specific Pokémon in ASCII art *) let
   print_pokemon_in_ascii filename pokemon_index = try let ascii_art_list =
   load_ascii_csv filename in (* Fetch and print the specified Pokémon's ASCII
   art *) let ascii_art = List.nth ascii_art_list pokemon_index in List.iter
   print_endline ascii_art with | Failure _ -> print_endline "Invalid Pokémon\n
   index." | _ -> print_endline "Error loading or parsing ASCII file."

   (* Main function *) let () = let filename = "data/ascii.csv" in let
   pokemon_index = 0 (* Change this index to select the desired Pokémon *) in
   print_pokemon_in_ascii filename pokemon_index (* let print_pokemon_in_ascii =
   (*100 characters per row*) let ascii_file = Csv.load "data/ascii.csv" in
   *) *)

(** Game loop for a single player*)
let play = main_menu ()
(* let () = Random.self_init () (* let create_AI_team lvl = let team = *)

   (* let rec game_loop team1 team2 decision = match decision with *)

   (*Basic driver program*) let play () = print_endline "Start playing"

   let build_team () = let rec add_pokemon curr_team = print_string "Choose a
   species: "; let spec = read_line () in print_string "Choose a nature: "; let
   nat = read_line () in try let poke = create spec 1 nat in let new_team = poke
   :: curr_team in print_endline ("Your level 1 " ^ nat ^ " " ^ spec ^ " was
   added to the team!"); if List.length new_team >= 4 then ( print_endline
   "You've created a full team!"; print_endline (String.concat ", " (List.map
   species new_team)); new_team) else add_pokemon new_team with BadPokemon ->
   print_endline "That is not a valid Pokemon. Try again."; add_pokemon
   curr_team in add_pokemon []

   let show_help_menu () = print_endline "Help menu"

   let () = print_endline "Welcome to Pokemon!";

   let rec choose () = print_endline "\n\ Press p to start playing with the
   default team.\n\ Press t to build a team of your choice.\n\ Press h for
   help."; let input = read_line () in if input = "p" then play () else if input
   = "t" then let _ = build_team () in choose () else if input = "h" then
   show_help_menu () else ( print_endline "Please press p, t, or h."; choose ())
   in choose () *)
