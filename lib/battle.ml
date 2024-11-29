open Pokemon

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
let pick_team () : team =
  let rec add_pokemon (curr_team : team) =
    print_string "Choose a species: ";
    let species = read_line () in
    print_string "Choose a nature: ";
    let nature = read_line () in
    try
      let pokemon = Pokemon.create species 50 nature in
      let new_team = pokemon :: curr_team in
      print_endline ("Added " ^ species);
      if List.length new_team >= 6 then (
        print_endline "Created team!";
        new_team)
      else add_pokemon new_team
    with Pokemon.BadPokemon ->
      print_endline "Invalid species!";
      add_pokemon curr_team
  in
  add_pokemon []

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
  print_string "Choose an action: \n1. Attack\n2. Switch\n3. UseItem\n4. Run\n";
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

let rec battle_loop (battle : battle_state) = failwith "TODO"

let rec main_menu () =
  print_endline
    "Welcome to Pokemon Gen 1 Battle Simulator!\n1. Play against AI\n2. Exit\n";
  match read_line () with
  | "1" | "Play" | "play" | "p" ->
      print_endline "Starting the game...";
      let team1 = pick_team () in
      let team2 = create_ai_team () in
      let battle_state = init_battle team1 team2 in
      battle_loop battle_state
  | "2" | "Exit" | "exit" | "e" -> exit 0
  | _ -> main_menu ()
