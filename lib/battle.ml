open Pokemon
open ANSITerminal

let every_pokemon () =
  let rows = Csv.load "lib/python/data/first_151_pokemon.csv" in
  List.map (fun row -> List.nth row 1) rows

(* Type definitions *)
type team = t list

(* Type for a team of Pokémon *)
type decision =
  | Attack of string
  | Switch of string
  | UseItem of string
  | Run

(* Type for the decision that a player can make *)

type battle_state = {
  team1 : team;
  team2 : team;
  current_turn : int;
  status : battle_status;
  current_pokemon : Pokemon.t * Pokemon.t;
}

and battle_status =
  | Idle
  | PlayerTurn
  | AITurn
  | Ended

(* Type for the current state of the battle *)

(* Functions *)

let init_battle (team1 : team) (team2 : team) : battle_state =
  match (team1, team2) with
  | p1 :: _, p2 :: _ ->
      {
        team1;
        team2;
        current_turn = 0;
        status = Idle;
        current_pokemon = (p1, p2);
      }
  | _ ->
      failwith "Both teams must have at least one Pokémon to start the battle."

let create_random_team () : team = failwith "TODO"
let create_ai_team () : team = failwith "TODO"
let choose_move () : move = failwith "TODO"

let rec attack_menu battle_state =
  let player_pokemon, _ = battle_state.current_pokemon in

  let moves = player_pokemon.moves in

  (* print out the list of moves *)
  print_endline "Choose an attack (by\n   number):";
  List.iteri
    (fun (i : int) (move : move) ->
      print_endline (string_of_int (i + 1) ^ ". " ^ move.name))
    moves;

  match read_line () with
  | choice when int_of_string_opt choice <> None ->
      let index = int_of_string choice - 1 in
      if index >= 0 && index < List.length moves then
        Attack (List.nth moves index).name
      else (
        print_endline "Invalid choice. Try again.";
        attack_menu battle_state)
  | _ ->
      print_endline "Invalid input. Try again.";
      attack_menu battle_state

let rec switch_menu () = failwith "TODO"
let rec use_item_menu () = failwith "TODO"

let rec get_player_action () =
  print_string []
    "Choose an action: \n1. Attack\n2. Switch\n3. UseItem\n4. Run\n";
  match read_line () with
  | "1" | "Attack" | "attack" | "a" -> Attack ""
  | "2" | "Switch" | "switch" | "s" -> Switch ""
  | "3" | "UseItem" | "useitem" | "u" -> UseItem ""
  | "4" | "Run" | "run" | "r" -> Run
  | _ ->
      print_endline "Invalid action! Please try again.";
      get_player_action ()

let rec handle_player_action (action : decision) (battle : battle_state) :
    battle_state =
  match action with
  (* placeholder to just return a decision *)
  | Attack "" ->
      let next_action = attack_menu battle in
      handle_player_action next_action battle
  | Attack move_name ->
      (* TODO handle logic for what a move would do and switch pokemon *)
      print_endline ("You used the move " ^ move_name ^ "!");
      { battle with status = AITurn }
  | Switch "" ->
      let next_action = switch_menu () in
      handle_player_action next_action battle
  | Switch pokemon_name ->
      print_endline ("You switched to " ^ pokemon_name ^ "!");
      { battle with status = AITurn }
  | UseItem "" ->
      let next_action = use_item_menu () in
      handle_player_action next_action battle
  | UseItem item_name ->
      print_endline ("You used " ^ item_name ^ "!");
      { battle with status = AITurn }
  | Run ->
      print_endline "You ran away!";
      { battle with status = Ended }

let make_ai_action (battle : battle_state) : decision =
  Run (* Placeholder implementation *)

let handle_player_decision (decision : decision) (battle : battle_state) :
    battle_state =
  { battle with current_turn = battle.current_turn + 1 }

let battle_loop (battle : battle_state) =
  print_endline "Team1: ";
  let pokemon = List.map pokemon_to_string battle.team1 in
  let pokemon_string = List.fold_left ( ^ ) "" pokemon in
  print_endline pokemon_string

(*HELPER FUNCTIONS FOR THE main_menu and pick_team FUNCTIONS !!! (for terminal
  UI)*)

let clear_screen () = print_string [] "\027[2J"

let rec configure_pokemon pokemon =
  clear_screen ();
  display_learnable_moves pokemon.species;
  print_endline "1. Add move";
  print_endline "2. Remove a move";
  print_endline "3. Proceed";
  print_endline "4. Back";
  print_endline "5. Exit";

  match read_line () with
  | "1" | "Add" | "Add move" | "add" | "a" ->
      if List.length pokemon.moves >= 4 then (
        print_endline
          "You already have 4 moves. Remove one before adding another.";
        ignore (read_line ());
        configure_pokemon pokemon)
      else (
        print_endline "Enter the name of the move to add: ";
        let move_name = read_line () in
        let move = create_move_from_name move_name in
        let updated_pokemon = { pokemon with moves = move :: pokemon.moves } in
        ignore (read_line ());
        configure_pokemon updated_pokemon)
  | "2" | "Remove" | "remove" | "r" ->
      if List.length pokemon.moves = 0 then (
        print_endline "No moves to remove.";
        ignore (read_line ());
        configure_pokemon pokemon)
      else (
        print_endline "Enter the name of the move to remove:";
        let move_name = read_line () in
        let updated_moves =
          List.filter (fun m -> m.name <> move_name) pokemon.moves
        in
        let updated_pokemon = { pokemon with moves = updated_moves } in
        print_endline ("Removed " ^ move_name);
        ignore (read_line ());
        configure_pokemon updated_pokemon)
  | "3" | "Proceed" | "proceed" | "p" ->
      print_endline "Finalizing...";
      pokemon
  | "4" | "Back" | "back" | "b" ->
      print_endline "Returning to previous menu";
      pokemon
  | "5" | "Exit" | "exit" | "e" -> exit 0
  | _ ->
      print_endline "Invalid option. Try again.";
      ignore (read_line ());
      configure_pokemon pokemon

let rec poke_info_screen all_pokemon name : Pokemon.t * bool =
  clear_screen ();
  let pokemon = create name 50 "hardy" in
  print_endline ("Species: " ^ pokemon.species ^ "\n\n");
  display_learnable_moves pokemon.species;
  print_endline "1. Select pokemon to add moves";
  print_endline "2. Back";
  print_endline "3. Exit";
  match read_line () with
  | "1" | "Select" | "select" | "s" -> (configure_pokemon pokemon, true)
  | "2" | "Back" | "back" | "b" -> (pokemon, false)
  | "3" | "Exit" | "exit" | "e" -> exit 0
  | _ ->
      print_endline "Invalid option. Try again.";
      ignore (read_line ());
      poke_info_screen all_pokemon pokemon.species

let rec search_for_pokemon (curr_team : team) all_pokemon : team =
  let rec loop query =
    clear_screen ();
    print_endline "Search for a Pokemon...";
    print_endline ("Current query: " ^ query ^ "\n");

    let filtered =
      List.filter
        (fun name -> String.starts_with ~prefix:query name)
        all_pokemon
    in
    if List.length filtered = 0 then print_endline "No Pokemon found"
    else (
      print_endline "Matching Pokemon:";
      List.iteri
        (fun i name -> print_endline (string_of_int (i + 1) ^ ". " ^ name))
        filtered);

    print_endline "\nOptions:";
    print_endline "- Type more letters to refine your search.";
    print_endline "- Type the number of the Pokemon to add it to your team";
    print_endline "- Type 'back' to return to the selection menu.";
    print_endline "- Press Enter to reset the query.";

    match read_line () with
    | "" -> loop ""
    | "back" -> curr_team
    | input ->
        if int_of_string_opt input <> None then
          let index = int_of_string input - 1 in
          if index >= 0 && index < List.length filtered then
            let selected_name = List.nth filtered index in
            let pokemon, add_to_team =
              poke_info_screen all_pokemon selected_name
            in
            if add_to_team then
              let updated_team = pokemon :: curr_team in
              if List.length updated_team > 6 then (
                print_endline "Team is full! Returning to search.";
                ignore (read_line ());
                loop query)
              else search_for_pokemon updated_team all_pokemon
            else search_for_pokemon curr_team all_pokemon
          else (
            print_endline "Invalid selection. Try again";
            ignore (read_line ());
            loop query)
        else loop input
  in
  loop ""

let view_team (curr_team : team) : unit = failwith "TODO"

let rec show_menu (curr_team : team) all_pokemon : team =
  clear_screen ();
  print_endline "Pokemon Selection Menu:";
  print_endline "1. Search for a Pokemon";
  print_endline "2. View team";
  print_endline "3. Confirm team and start";
  print_endline "4. Back to main menu";
  print_endline "5. Exit";
  match read_line () with
  | "1" | "Search" | "search" | "s" ->
      let updated_team = search_for_pokemon curr_team all_pokemon in
      show_menu updated_team all_pokemon
  | "2" | "View" | "view" | "v" ->
      view_team curr_team;
      show_menu curr_team all_pokemon
  | "3" | "Confirm" | "confirm" | "c" ->
      if List.length curr_team >= 1 then (
        print_endline "Team confirmed! Starting the game...";
        curr_team)
      else (
        print_endline "You need at least one pokemon on your team to begin!";
        show_menu curr_team all_pokemon)
  | "4" | "Back" | "back" | "b" -> curr_team
  | "5" | "Exit" | "exit" | "e" -> exit 0
  | _ ->
      print_endline "Invalid choice! Please try again.";
      show_menu curr_team all_pokemon

let pick_team all_pokemon : team =
  let initial_team = [] in
  show_menu initial_team all_pokemon

let rec main_menu () =
  print_endline
    "Welcome to Pokemon Gen 1 Battle Simulator!\n1. Play against AI\n2. Exit\n";
  let all_pokemon = every_pokemon () in
  match read_line () with
  | "1" | "Play" | "play" | "p" ->
      print_endline "Starting the game...";
      let team1 = pick_team all_pokemon in
      (* TODO let team2 = create_ai_team () in *)
      let battle_state = init_battle team1 [] in
      battle_loop battle_state
  | "2" | "Exit" | "exit" | "e" -> exit 0
  | _ -> main_menu ()
