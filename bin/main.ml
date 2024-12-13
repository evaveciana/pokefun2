open Pokefun.Pokemon
open ANSITerminal

let every_pokemon =
  let rows = Csv.load "data/first_151_pokemon.csv" in
  List.map (fun row -> List.nth row 1) rows

type team = t list
(* Type for a team of Pokémon *)

(*Type for the decisions a player can make*)
type decision =
  | Attack
  | Switch
  | Run

(*Type for the state of the battle*)
type battle_state = {
  team1 : team;
  team2 : team;
  current_turn : int;
  status : battle_status;
  current_pokemon : t * t;
}

and battle_status =
  | PlayerTurn
  | Team1Win
  | Team2Win

let () = Random.self_init ()
let roll chance = Random.int 100 < chance

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

let print_effectiveness (move : move) (pokemon : t) : unit =
  match calc_effectiveness_mult move pokemon with
  | 0.0 -> print_endline "It had no effect."
  | 0.5 -> print_endline "It was not very effective..."
  | 2.0 -> print_endline "It was super effective!"
  | _ -> ()

let rec add_moves p moves =
  match moves with
  | [] -> p
  | h :: t -> add_pokemon_move (add_moves p t) h

let create_fake_team_ai () : team =
  [
    add_moves
      (create "lapras" 55 "naughty")
      [ "ice-beam"; "thunderbolt"; "psychic"; "confuse-ray" ];
    add_moves
      (create "mewtwo" 55 "mild")
      [ "psychic"; "recover"; "barrier"; "swift" ];
  ]

let create_player_teams () : team =
  let team1 =
    [
      add_moves
        (create "lapras" 55 "naughty")
        [ "ice-beam"; "thunderbolt"; "psychic"; "confuse-ray" ];
      add_moves
        (create "mewtwo" 55 "mild")
        [ "psychic"; "recover"; "barrier"; "swift" ];
      add_moves
        (create "snorlax" 55 "adamant")
        [ "rest"; "hyper-beam"; "earthquake"; "headbutt" ];
      add_moves
        (create "charizard" 55 "hardy")
        [ "fly"; "slash"; "earthquake"; "fire-spin" ];
      add_moves
        (create "dragonite" 55 "sassy")
        [ "dragon-rage"; "blizzard"; "surf"; "thunderbolt" ];
      add_moves
        (create "venusaur" 55 "jolly")
        [ "solar-beam"; "sleep-powder"; "leech-seed"; "growth" ];
    ]
  in
  let team2 =
    [
      add_moves
        (create "starmie" 55 "timid")
        [ "surf"; "psychic"; "thunderbolt"; "recover" ];
      add_moves
        (create "rhydon" 55 "adamant")
        [ "earthquake"; "rock-slide"; "stomp"; "body-slam" ];
      add_moves
        (create "chansey" 55 "calm")
        [ "thunder-wave"; "soft-boiled"; "ice-beam"; "seismic-toss" ];
      add_moves
        (create "jolteon" 55 "modest")
        [ "thunderbolt"; "pin-missile"; "quick-attack"; "agility" ];
      add_moves
        (create "cloyster" 55 "bold")
        [ "surf"; "blizzard"; "explosion"; "rest" ];
      add_moves
        (create "alakazam" 55 "timid")
        [ "psychic"; "thunder-wave"; "recover"; "reflect" ];
    ]
  in
  let team3 =
    [
      add_moves
        (create "gengar" 55 "hasty")
        [ "hypnosis"; "thunderbolt"; "psychic"; "explosion" ];
      add_moves
        (create "dragonite" 55 "naive")
        [ "hyper-beam"; "earthquake"; "blizzard"; "fire-blast" ];
      add_moves
        (create "exeggutor" 55 "relaxed")
        [ "psychic"; "stun-spore"; "sleep-powder"; "explosion" ];
      add_moves
        (create "tauros" 55 "jolly")
        [ "body-slam"; "earthquake"; "hyper-beam"; "blizzard" ];
      add_moves
        (create "zapdos" 55 "timid")
        [ "thunderbolt"; "drill-peck"; "agility"; "light-screen" ];
      add_moves
        (create "snorlax" 55 "adamant")
        [ "body-slam"; "hyper-beam"; "rest"; "earthquake" ];
    ]
  in
  List.nth [ team1; team2; team3 ] (Random.int 3)

let create_ai_team () : team =
  [
    add_moves
      (create "pidgeot" 59 "naughty")
      [ "wing-attack"; "gust"; "take-down"; "sky-attack" ];
    add_moves
      (create "alakazam" 57 "mild")
      [ "psybeam"; "psychic"; "recover"; "hyper-beam" ];
    add_moves
      (create "rhydon" 59 "adamant")
      [ "horn-drill"; "leer"; "earthquake"; "rock-slide" ];
    add_moves
      (create "exeggutor" 59 "hardy")
      [ "stomp"; "psychic"; "petal-dance"; "mega-drain" ];
    add_moves
      (create "gyarados" 61 "sassy")
      [ "dragon-rage"; "hydro-pump"; "hyper-beam"; "waterfall" ];
    add_moves
      (create "charizard" 63 "jolly")
      [ "fire-blast"; "slash"; "sky-attack"; "flamethrower" ];
  ]

let view_team (curr_team : team) : unit =
  if List.length curr_team = 0 then print_endline "Team is empty."
  else (
    print_endline "Viewing team:";
    List.iteri
      (fun i poke ->
        let poke_str = poke.species in
        print_endline (string_of_int (i + 1) ^ ". " ^ poke_str))
      curr_team)

let lose () =
  print_endline "You lose! You suck";
  exit 0

let win () =
  print_endline "You win!";
  exit 0

let rec attack_menu battle_state =
  let player_pokemon, ai_pokemon = battle_state.current_pokemon in

  let moves = player_pokemon.moves in

  (* print out the list of moves *)
  print_endline
    (player_pokemon.species ^ " ("
    ^ string_of_int (cur_hp player_pokemon)
    ^ " hp) vs " ^ ai_pokemon.species ^ " ("
    ^ string_of_int (cur_hp ai_pokemon)
    ^ " hp)");
  print_endline "Choose an attack (by number): \n";
  List.iteri
    (fun (i : int) (move : move) ->
      print_endline (string_of_int (i + 1) ^ ". " ^ move.name))
    moves;

  match read_line () with
  | choice when int_of_string_opt choice <> None ->
      let index = int_of_string choice - 1 in
      if index >= 0 && index < List.length moves then List.nth moves index
      else (
        print_endline "Invalid choice. Try again.";
        attack_menu battle_state)
  | _ ->
      print_endline "Invalid input. Try again.";
      attack_menu battle_state

let rec switch_menu (battle : battle_state) : t =
  print_endline "Choose a Pokemon by id to switch to: ";

  let current_team = battle.team1 in
  view_team current_team;

  let switch_to_id = int_of_string (read_line ()) in
  let pokemon_to_switch_to = List.nth current_team (switch_to_id - 1) in
  if cur_hp pokemon_to_switch_to <= 0 then (
    print_endline "That pokemon is already fainted!";
    switch_menu battle)
  else pokemon_to_switch_to

(*for a pokemon on the team print out all the stats such as Pokemon name, its
  tipe, and HP. Used after a player makes move*)
let print_pokemon_stats pokemon =
  print_endline ("Name: " ^ pokemon.species);
  let types =
    if pokemon.is_dual_type then fst pokemon.tipe ^ " / " ^ snd pokemon.tipe
    else fst pokemon.tipe
  in
  print_endline ("Type: " ^ types);
  print_endline ("Level: " ^ string_of_int pokemon.level);
  print_endline
    ("HP: "
    ^ string_of_int pokemon.cur_hp
    ^ "/"
    ^ string_of_int (cur_hp pokemon))

(*for a pokemon on the team print out all the stats such as Pokemon name, its
  tipe, and HP, def, atk etc.*)
let print_all_pokemon_stats pokemon : unit =
  print_endline ("Name: " ^ pokemon.species);
  let types =
    if pokemon.is_dual_type then fst pokemon.tipe ^ " / " ^ snd pokemon.tipe
    else fst pokemon.tipe
  in
  print_endline ("Type: " ^ types);
  print_endline ("Level: " ^ string_of_int pokemon.level);
  print_endline
    ("HP: "
    ^ string_of_int pokemon.cur_hp
    ^ "/"
    ^ string_of_int (cur_hp pokemon));
  print_endline ("Attack: " ^ string_of_int (atk pokemon));
  print_endline ("Defense: " ^ string_of_int (def pokemon));
  print_endline ("Speed: " ^ string_of_int (spd pokemon));
  print_endline ("Special Attack: " ^ string_of_int (spatk pokemon));
  print_endline ("Special Defense: " ^ string_of_int (spdef pokemon));
  print_endline ("Nature: " ^ pokemon.nature)

let rec get_player_action () : decision =
  print_string [] "Choose an action: \n1. Attack\n2. Switch\n3. Quit\n";
  match read_line () with
  | "1" | "Attack" | "attack" | "a" -> Attack
  | "2" | "Switch" | "switch" | "s" -> Switch
  | "3" | "Run" | "run" | "r" -> Run
  | _ ->
      print_endline "Invalid action! Please try again.";
      get_player_action ()

let print_string_list lst =
  let rec aux = function
    | [] -> ()
    | [ x ] ->
        Printf.printf "%s\n"
          x (* Print the last element without a trailing comma *)
    | x :: xs ->
        Printf.printf "%s, " x;
        aux xs
  in
  Printf.printf "[";
  (* Start the list formatting *)
  aux lst;
  Printf.printf "]\n" (* End the list formatting *)

let get_ai_move (battle : battle_state) : move =
  let player_poke, ai_poke = battle.current_pokemon in

  let moves = ai_poke.moves in
  match moves with
  | [ move1; move2; move3; move4 ] ->
      let hps =
        List.map
          (fun move -> calc_damage ai_poke player_poke move)
          [ move1; move2; move3; move4 ]
      in
      let moves_with_hps = List.combine moves hps in
      fst
        (List.fold_left
           (fun (best_move, best_score) (move, score) ->
             if score < best_score then (move, score)
             else (best_move, best_score))
           (move1, min_int) moves_with_hps)
  | _ ->
      print_string_list (List.map (fun m -> m.name) moves);
      failwith "The moves list must contain exactly 4 elements."

let make_ai_move (battle : battle_state) (ai_move : move) : battle_state =
  let player_poke, ai_poke = battle.current_pokemon in
  let ai_poke, player_poke =
    print_endline (ai_poke.species ^ " used " ^ ai_move.name ^ "!");
    print_effectiveness ai_move ai_poke;

    attack ai_poke player_poke ai_move
  in

  {
    battle with
    current_pokemon = (player_poke, ai_poke);
    current_turn = battle.current_turn + 1;
  }

let make_player_move battle player_poke ai_poke player_move =
  let player_poke, ai_poke =
    print_endline (player_poke.species ^ " used " ^ player_move.name ^ "!");
    print_effectiveness player_move player_poke;
    attack player_poke ai_poke player_move
  in

  {
    battle with
    current_pokemon = (player_poke, ai_poke);
    current_turn = battle.current_turn + 1;
  }

let make_player_switch battle pokemon =
  let _, ai_poke = battle.current_pokemon in
  print_endline ("You sent out " ^ pokemon.species ^ "!");
  { battle with current_pokemon = (pokemon, ai_poke) }

let get_ai_switch battle =
  let alive_pokemon = List.filter (fun p -> p.cur_hp > 0) battle.team2 in
  let p = List.hd alive_pokemon in

  print_endline ("Opponent sent out " ^ p.species ^ "!");
  p

let replace_pokemon team pokemon =
  List.map (fun p -> if p.species = pokemon.species then pokemon else p) team

let update_team battle =
  let player_poke, ai_poke = battle.current_pokemon in
  {
    battle with
    team1 = replace_pokemon battle.team1 player_poke;
    team2 = replace_pokemon battle.team2 ai_poke;
  }

let check_status (battle : battle_state) =
  (* print_endline "check_status"; *)
  (* print_battle_status battle; *)
  let fainted_team1 = List.filter (fun p1 -> p1.cur_hp <= 0) battle.team1 in
  (* let p1 = List.hd battle.team1 in let p2 = List.hd battle.team2 in *)
  let fainted_team2 = List.filter (fun p2 -> p2.cur_hp <= 0) battle.team2 in

  if List.length fainted_team1 = List.length battle.team1 then lose ()
  else if List.length fainted_team2 = List.length battle.team2 then win ()

let check_deaths battle =
  check_status (update_team battle);
  let player_poke, ai_poke = battle.current_pokemon in

  let battle, switched =
    if cur_hp player_poke <= 0 then
      let () = print_endline (player_poke.species ^ " has fainted!") in
      ({ battle with current_pokemon = (switch_menu battle, ai_poke) }, true)
    else (battle, false)
  in
  if cur_hp ai_poke <= 0 then
    let player_poke, _ = battle.current_pokemon in
    let () = print_endline (ai_poke.species ^ " has fainted!") in
    ({ battle with current_pokemon = (player_poke, get_ai_switch battle) }, true)
  else (battle, switched)

let update_both_stats battle =
  let player_poke, ai_poke = battle.current_pokemon in
  {
    battle with
    current_pokemon =
      (update_current_stats player_poke, update_current_stats ai_poke);
  }

let rec handle_action (action : decision) (ai_move : move)
    (battle : battle_state) : battle_state =
  let battle = update_both_stats battle in
  let battle =
    match action with
    | Attack ->
        let player_move = attack_menu battle in

        let player_poke, ai_poke = battle.current_pokemon in
        let player_goes_first =
          if player_move.priority < ai_move.priority then false
          else if player_move.priority > ai_move.priority then true
          else if spd player_poke < spd ai_poke then false
          else if spd player_poke > spd ai_poke then true
          else roll 50
        in
        let new_battle =
          if player_goes_first then
            let battle =
              make_player_move (update_team battle) player_poke ai_poke
                player_move
            in

            let battle, fainted = check_deaths (update_team battle) in

            if fainted then battle else make_ai_move battle ai_move
          else
            let battle = make_ai_move battle ai_move in
            let battle, fainted = check_deaths (update_team battle) in
            if fainted then battle
            else
              make_player_move (update_team battle) player_poke ai_poke
                player_move
        in

        fst (check_deaths new_battle)
    | Switch ->
        let switch = switch_menu battle in
        if switch <> fst battle.current_pokemon then (
          let battle = make_player_switch battle switch in
          print_endline ("You switched to " ^ switch.species ^ "!");
          let battle =
            make_ai_move
              { battle with current_turn = battle.current_turn + 1 }
              ai_move
          in
          fst (check_deaths (update_team battle)))
        else handle_action (get_player_action ()) ai_move battle
    | Run ->
        print_endline "You ran away!";
        { battle with status = Team2Win }
  in
  update_team battle

let handle_player_decision (decision : decision) (battle : battle_state) :
    battle_state =
  { battle with current_turn = battle.current_turn + 1 }

(* JUST FOR implementation! initializes teams for displaying menu *)
let setup_fake () : battle_state =
  let team1 = create_ai_team () in
  let team2 = create_fake_team_ai () in
  let battle_state = init_battle team1 team2 in
  battle_state

let rec battle_loop (battle : battle_state) =
  match battle.status with
  | PlayerTurn ->
      let player_decision = get_player_action () in
      let ai_decision = get_ai_move battle in
      let new_state = handle_action player_decision ai_decision battle in

      battle_loop new_state
  | Team1Win -> win ()
  | Team2Win -> lose ()

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

let rec poke_info_screen all_pokemon name : t * bool =
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
      print_endline "Press Enter to continue...";
      ignore (read_line ());
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
    "Welcome to Pokemon Gen 1 Battle Simulator!\n\
     1. Pick your own team\n\
     2. Play with a preset team\n\
     3. Exit\n";
  let all_pokemon = every_pokemon in
  match read_line () with
  | "1" | "Play" | "play" | "p" ->
      print_endline "Starting the game...";
      let team1 = pick_team all_pokemon in
      print_endline "pick_team finished";
      let team2 = create_ai_team () in
      print_endline "create_random_team finished";
      let battle_state = init_battle team1 team2 in
      print_endline "init_battle finished";
      battle_loop battle_state
  | "2" ->
      let team1 = create_player_teams () in
      let team2 = create_ai_team () in
      battle_loop (init_battle team1 team2)
  | "3" | "Exit" | "exit" | "e" -> exit 0
  | _ -> main_menu ()

(* Main entry point *)
let play = battle_loop (setup_fake ())
