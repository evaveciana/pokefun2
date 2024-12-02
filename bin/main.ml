open Pokefun.Pokemon
open Pokefun.Battle

(** Game loop for a single player*)
let play = test_display ()
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
