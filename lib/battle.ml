open Pokemon
open ANSITerminal
open Tui

let every_pokemon () =
  let rows = Csv.load "lib/python/data/first_151_pokemon.csv" in
  List.map (fun row -> List.nth row 1) rows

(* Type definitions *)
type team = t list

(* Type for a team of Pokémon *)
type decision =
  | Attack of string
  | Switch of string
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
  | PlayerTurn
  | AITurn
  | Team1Win
  | Team2Win

(* Type for the current state of the battle *)

(* Functions *)

let init_battle (team1 : team) (team2 : team) : battle_state =
  match (team1, team2) with
  | p1 :: _, p2 :: _ ->
      {
        team1;
        team2;
        current_turn = 0;
        status = PlayerTurn;
        current_pokemon = (p1, p2);
      }
  | _ ->
      failwith "Both teams must have at least one Pokémon to start the battle."

let create_random_team () : team =
  let level = 50 in
  let random_nature =
    let natures_csv = Csv.load "data/multipliers_by_nature" in
    let chosen_row =
      List.nth natures_csv (Random.int (List.length natures_csv))
    in
    List.hd chosen_row
  in

  let random_species =
    let species_list = every_pokemon () in
    List.nth species_list (Random.int (List.length species_list))
  in
  let random_moves species =
    let moves = get_moves species in
    let rec pick_moves n moves acc =
      if n = 0 then acc
      else
        let move = List.nth moves (Random.int (List.length moves)) in
        pick_moves (n - 1) moves (move :: acc)
    in
    pick_moves 4 moves []
  in

  let create_random_pokemon species =
    let level = level in
    let nature = random_nature in
    let pokemon = create species level nature in
    let moves = random_moves species in
    let updated_pokemon =
      List.fold_left (fun p move -> add_pokemon_move p move.id) pokemon moves
    in
    updated_pokemon
  in

  let rec generate_team n acc =
    if n = 6 then acc
    else
      let species = random_species in
      let pokemon = create_random_pokemon species in
      generate_team (n + 1) (pokemon :: acc)
  in
  generate_team 0 []

(*for now just generate random team, may change later*)
let create_ai_team () : team = create_random_team ()

(*in battle choosing a move*)
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

let rec get_player_action () =
  print_string []
    "Choose an action: \n1. Attack\n2. Switch\n3. UseItem\n4. Run\n";
  match read_line () with
  | "1" | "Attack" | "attack" | "a" -> Attack ""
  | "2" | "Switch" | "switch" | "s" -> Switch ""
  | "3" | "Run" | "run" | "r" -> Run
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
      if battle.status = PlayerTurn then (
        let attacker, defender = battle.current_pokemon in
        print_endline (attacker.species ^ " used " ^ move_name ^ "!");

        let new_attacker, new_defender =
          attack attacker defender (create_move_from_name move_name)
        in
        {
          battle with
          current_pokemon = (new_attacker, new_defender);
          status = AITurn;
          current_turn = battle.current_turn + 1;
        })
      else
        (* switch for handling ai move *)
        let defender, attacker = battle.current_pokemon in
        print_endline (attacker.species ^ " used " ^ move_name ^ "!");
        let new_attacker, new_defender =
          attack attacker defender (create_move_from_name move_name)
        in
        {
          battle with
          current_pokemon = (new_defender, new_attacker);
          status = PlayerTurn;
          current_turn = battle.current_turn + 1;
        }
  | Switch "" ->
      let next_action = switch_menu () in
      handle_player_action next_action battle
  | Switch pokemon_name ->
      print_endline ("You switched to " ^ pokemon_name ^ "!");
      { battle with status = AITurn; current_turn = battle.current_turn + 1 }
  | Run ->
      print_endline "You ran away!";
      { battle with status = Team2Win }

let make_ai_action (battle : battle_state) : decision =
  Run (* Placeholder implementation *)

let handle_player_decision (decision : decision) (battle : battle_state) :
    battle_state =
  { battle with current_turn = battle.current_turn + 1 }

(* JUST FOR implementation! initializes teams for displaying menu *)
let setup_fake () : battle_state =
  let p1 =
    {
      species = "venusaur";
      is_dual_type = true;
      tipe = ("Grass", "Poison");
      base_stats = zero_stats;
      cur_stats = zero_stats;
      stat_stages = zero_stats;
      moves = [ example_move () ];
      level = 50;
      ailment = "healthy";
      nature = "hardy";
      cur_hp = 1;
    }
  in
  let team1 = [ p1 ] in
  let team2 = [ p1 ] in
  let battle_state = init_battle team1 team2 in
  battle_state

let check_status (battle : battle_state) : battle_state =
  let fainted_team1 = List.filter (fun p1 -> p1.cur_hp <= 0) battle.team1 in
  let fainted_team2 = List.filter (fun p2 -> p2.cur_hp <= 0) battle.team2 in
  if List.length fainted_team1 = List.length battle.team1 then
    { battle with status = Team2Win }
  else if List.length fainted_team2 = List.length battle.team2 then
    { battle with status = Team1Win }
  else battle

let rec battle_loop (battle : battle_state) =
  match battle.status with
  | PlayerTurn ->
      let decision = get_player_action () in
      let new_state = handle_player_action decision battle in
      battle_loop new_state
  | AITurn ->
      let ai_decision = make_ai_action battle in
      let new_state = handle_player_decision ai_decision battle in
      battle_loop new_state
  | Team1Win ->
      print_endline "You win! Congrats!";
      Unix.sleepf 3.0;
      exit 0
  | Team2Win ->
      print_endline "You lost! Sorry bro";
      Unix.sleepf 3.0;
      exit 0

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
  let all_pokemon = List.tl all_pokemon in
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
      let team2 = create_random_team () in
      let battle_state = init_battle team1 team2 in
      battle_loop battle_state
  | "2" | "Exit" | "exit" | "e" -> exit 0
  | _ -> main_menu ()
