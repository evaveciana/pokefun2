open OUnit2
open Pokefun.Pokemon
open Pokefun.Tui

let make_stats_test (name, f, input, expected) =
  name >:: fun _ -> assert_equal expected (f input) ~printer:string_of_int

let dual_pokemon = create "bulbasaur" 1 "hardy"
let non_dual_pokemon = create "charmander" 5 "quiet"
let stats = [ "hp"; "atk"; "spatk"; "def"; "spdef"; "spd"; "acc"; "eva" ]

let make_stat_change_test stat =
  "Apply stat change" ^ stat >:: fun _ ->
  assert_equal
    [ 12; 6; 6; 6; 6; 6; 100; 100 ]
    (stats_to_list (cur_stats (apply_stat_change dual_pokemon stat 0)))
    ~printer:(fun x -> String.concat ", " (List.map string_of_int x))

let stat_change_tests = List.map make_stat_change_test stats

let pokemon_stats_tests =
  [
    (* ("Base HP", base_hp, dual_pokemon, 45); *)
    (* ("Base attack", base_atk, dual_pokemon, 49); *)
    (* ("Base special attack", base_spatk, dual_pokemon, 65); *)
    (* ("Base defense", base_def, dual_pokemon, 49); *)
    (* ("Base special defense", base_spdef, dual_pokemon, 65); *)
    (* ("Base speed", base_spd, dual_pokemon, 45); *)
    (* ("HP", hp, dual_pokemon, 12); *)
    ("Max hp", max_hp, dual_pokemon, 12);
    ("Attack", atk, dual_pokemon, 6);
    ("Special attack", spatk, dual_pokemon, 6);
    ("Defense", def, dual_pokemon, 6);
    ("Special defense", spdef, dual_pokemon, 6);
    ("Speed", spd, dual_pokemon, 6);
  ]

let pokemon_stats_test_cases = List.map make_stats_test pokemon_stats_tests

let other_pokemon_tests =
  [
    ( "Bad create 1" >:: fun _ ->
      assert_raises
        (Failure "Attempting to match to column out of range one_match")
        (fun () -> create "eva" 1 "hardy") );
    ( "Bad create 2" >:: fun _ ->
      assert_raises BadPokemon (fun () -> create "bulbasaur" 0 "hardy") );
    ( "Bad create 3" >:: fun _ ->
      assert_raises
        (Failure
           "Attempting to match to column out of range whole_row \
            data/multipliers_by_nature.csv confused") (fun () ->
          create "bulbasaur" 1 "confused") );
    (* ( "Species 1" >:: fun _ -> assert_equal "bulbasaur" (species
       dual_pokemon) ~printer:Fun.id ); ( "Species 2" >:: fun _ -> assert_equal
       "charmander" (species non_dual_pokemon) ~printer:Fun.id ); ( "Base stats"
       >:: fun _ -> assert_equal [ 45; 49; 49; 65; 65; 45; 0; 0 ] (stats_to_list
       (base_stats dual_pokemon)) ~printer:(fun x -> String.concat ", "
       (List.map string_of_int x)) ); *)
    ( "Current stats/calc current stats" >:: fun _ ->
      assert_equal
        [ 12; 6; 6; 6; 6; 6; 100; 100 ]
        (stats_to_list (cur_stats dual_pokemon))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Cur hp" >:: fun _ ->
      assert_equal 12 (cur_hp dual_pokemon) ~printer:string_of_int );
    ( "Attack effect on attacker" >:: fun _ ->
      assert_equal (cur_hp dual_pokemon)
        (cur_hp
           (fst
              (attack
                 (add_pokemon_move dual_pokemon "take-down")
                 non_dual_pokemon
                 (create_move_from_name "take-down"))))
        ~cmp:(fun x y -> y - x <= 2 && y - x >= 1)
        ~printer:string_of_int );
    ( "Attack effect on defender" >:: fun _ ->
      assert_equal (cur_hp dual_pokemon)
        (cur_hp
           (snd
              (attack
                 (add_pokemon_move dual_pokemon "tackle")
                 non_dual_pokemon
                 (create_move_from_name "tackle"))))
        ~cmp:(fun x y -> float_of_int y /. float_of_int x = 0.9)
        ~printer:string_of_int );
    ( "Update current stats" >:: fun _ ->
      assert_equal non_dual_pokemon (update_current_stats non_dual_pokemon) );
    ( "Add move/moves/get move id from move" >:: fun _ ->
      assert_equal [ 14; 75; 33 ]
        (List.map get_move_id_from_move
           (moves
              (add_pokemon_move
                 (add_pokemon_move
                    (add_pokemon_move dual_pokemon "swords-dance")
                    "razor-leaf")
                 "tackle")))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Bad add move 1" >:: fun _ ->
      assert_raises
        (Failure
           "Attempting to match to column out of range whole_row \
            data/moves.csv Hi") (fun () -> add_pokemon_move dual_pokemon "Hi")
    );
    ( "Bad add move 2" >:: fun _ ->
      assert_raises (Failure "A Pokemon can only have up to 4 moves.")
        (fun () ->
          add_pokemon_move
            (add_pokemon_move
               (add_pokemon_move
                  (add_pokemon_move
                     (add_pokemon_move dual_pokemon "swords-dance")
                     "razor-leaf")
                  "tackle")
               "vine-whip")
            "poison-powder") );
    ( "Get move ID from name" >:: fun _ ->
      assert_equal 24
        (get_move_id_from_name "double-kick")
        ~printer:string_of_int );
    ( "Move to string/Create move from name" >:: fun _ ->
      assert_equal
        "vine-whip (Type: Grass, Power: 45, PP: 25, Accuracy: 100, Damage \
         Class: Special)"
        (move_to_string (create_move_from_name "vine-whip"))
        ~printer:Fun.id );
    ( "Pokemon to string" >:: fun _ ->
      assert_equal
        "bulbasaur: swords-dance (Type: Normal, Power: -1, PP: 20, Accuracy: \
         -1, Damage Class: Physical)"
        (pokemon_to_string (add_pokemon_move dual_pokemon "swords-dance"))
        ~printer:Fun.id );
    (* ( "Display learnable moves" >:: (display_learnable_moves (create
       "caterpie" 1 "hardy") ); *)
    (* (print_endline "pound (Type:\n\ \ Normal, Power: 40, PP: 35, Accuracy:
       100, Damage Class: \ Special)") ~printer:Fun.id ); *)
    ( "Get moves" >:: fun _ ->
      assert_equal
        [
          "substitute";
          "rest";
          "light-screen";
          "double-team";
          "toxic";
          "solar-beam";
          "swords-dance";
          "amnesia";
          "skull-bash";
          "sludge";
          "petal-dance";
          "double-edge";
          "growth";
          "razor-leaf";
          "take-down";
          "sleep-powder";
          "poison-powder";
          "vine-whip";
          "vine-whip";
          "growl";
          "tackle";
        ] (get_moves "bulbasaur") ~printer:(fun x ->
          List.fold_right (fun elt acc -> elt ^ ", " ^ acc) x "") );
    ( "Calc effectiveness mult" >:: fun _ ->
      assert_equal 0.5
        (calc_effectiveness_mult
           (create_move_from_name "vine-whip")
           non_dual_pokemon)
        ~printer:string_of_float );
    ( "Get pokemon ID" >:: fun _ ->
      assert_equal 161 (get_pokemon_id "sentret") ~printer:string_of_int );
    ( "Calc damage" >:: fun _ ->
      assert_equal 0
        (calc_damage dual_pokemon non_dual_pokemon
           (create_move_from_name "vine-whip"))
        ~cmp:(fun _ y -> y <= 3 && y >= 1)
        ~printer:string_of_int );
  ]

(* let mock_read_lines = ref []

   let mock_read_line () = match !mock_read_lines with | [] -> failwith "No more
   mock input available. Check the input sequence length." | hd :: tl ->
   mock_read_lines := tl; hd

   (* Helper Functions *) let mock_team () = [ add_moves (create "lapras" 50
   "naughty") [ "surf"; "ice-beam"; "quick-attack"; "tackle" ]; add_moves
   (create "charizard" 50 "jolly") [ "flamethrower"; "fly"; "quick-attack";
   "tackle" ]; ]

   let mock_battle_state () = let team1 = mock_team () in let team2 =
   create_ai_team () in init_battle team1 team2

   (* Tests *)

   (* Test init_battle *) let test_init_battle _ = let team1 = mock_team () in
   let team2 = create_ai_team () in let battle_state = init_battle team1 team2
   in assert_equal battle_state.current_turn 0; assert_equal battle_state.status
   PlayerTurn; assert_equal (fst battle_state.current_pokemon).species "lapras"

   let test_init_battle2 _ = let team1 = create_player_teams () in let team2 =
   create_player_teams () in let battle_state = init_battle team1 team2 in
   assert_equal battle_state.current_turn 0

   let test_init_battle3 _ = let team1 = create_player_teams () in let team2 =
   create_player_teams () in let battle_state = init_battle team1 team2 in
   assert_equal battle_state.current_turn 0

   let test_init_battle4 _ = let team1 = create_player_teams () in let team2 =
   create_ai_team () in let battle_state = init_battle team1 team2 in
   assert_equal battle_state.current_turn 0

   let test_init_battle_empty_teams _ = assert_raises (Failure "Both teams must
   have at least one Pokémon to start the battle.") (fun () -> init_battle []
   [])

   (* Test create_ai_team *) let test_create_ai_team _ = let team =
   create_ai_team () in assert_equal (List.length team) 6; List.iter (fun p ->
   assert_bool "Pokemon has moves" (List.length p.moves > 0)) team

   (* Test get_player_action *) (* let test_get_player_action_attack _ =
   mock_read_lines := [ "1" ]; let action = get_player_action () in assert_equal
   action Attack *)

   (* let test_get_player_action_switch _ = mock_read_lines := [ "2" ]; let
   action = get_player_action () in assert_equal action Switch *)

   (* let test_get_player_action_invalid _ = mock_read_lines := [ "invalid"; "1"
   ]; let action = get_player_action () in assert_equal action Attack *)

   (* Test attack_menu *) (* let test_attack_menu _ = let battle_state =
   mock_battle_state () in let player_pokemon, _ = battle_state.current_pokemon
   in let moves = player_pokemon.moves in mock_read_lines := [ "1" ]; let
   selected_move = attack_menu battle_state in assert_equal selected_move
   (List.nth moves 0) *)

   (* Test battle_loop *) (* let test_battle_loop_player_win _ = let team1 = [
   add_moves (create "charizard" 100 "jolly") [ "flamethrower"; "tackle";
   "quick-attack"; "ember" ]; ] in let team2 = [ add_moves (create "bulbasaur" 1
   "docile") [ "tackle"; "quick-attack"; "ember"; "vine-whip" ]; ] in let
   battle_state = init_battle team1 team2 in assert_raises Exit (fun () ->
   battle_loop battle_state) *)

   (* let test_battle_loop_ai_win _ = let team1 = [ add_moves (create
   "bulbasaur" 1 "docile") [ "tackle"; "quick-attack"; "ember"; "vine-whip" ]; ]
   in let team2 = [ add_moves (create "charizard" 100 "jolly") [ "flamethrower";
   "quick-attack"; "ember"; "vine-whip" ]; ] in let battle_state = init_battle
   team1 team2 in assert_raises Exit (fun () -> battle_loop battle_state) *)

   (* Test pick_team *) let test_pick_team _ = mock_read_lines := [ "1"; "2";
   "confirm" ]; let team = pick_team [ "lapras"; "charizard"; "mewtwo" ] in
   assert_bool "Team selected" (List.length team > 0)

   (* let test_real_battle _ = mock_read_lines := [ "1"; "2"; "2"; "4"; "1";
   "2"; "1"; "3"; "1"; "1"; "2"; "3"; "1"; "2"; "1"; "1"; "1"; "4"; "2"; "5";
   "3"; ];

   let () = battle_loop (setup_fake ()) in assert_equal 1 1 *)

   (* Test switch_menu *) (* let test_switch_menu _ = let battle_state =
   mock_battle_state () in mock_read_lines := [ "1" ]; let switch_to =
   switch_menu battle_state in assert_equal switch_to (List.nth
   battle_state.team1 0) *)*)

(* Test case for chunk_string function *)
let test_chunk_string _ =
  assert (chunk_string "abcdef" 2 = [ "ab"; "cd"; "ef" ]);
  assert (chunk_string "abc" 5 = [ "abc" ]);
  assert (chunk_string "a" 1 = [ "a" ]);
  assert (chunk_string "" 3 = []);
  assert (chunk_string "abcdefgh" 3 = [ "abc"; "def"; "gh" ]);
  print_endline "test_chunk_string passed."

(* Test case for load_ascii_csv function *)

let test_load_ascii_csv _ =
  (* Test with a simple CSV file *)
  let filename = "data/ascii.csv" in
  try
    let ascii_art = load_ascii_csv filename in
    assert (List.length ascii_art > 0);
    (* Ensure we have some ASCII art *)
    print_endline "test_load_ascii_csv passed."
  with
  | Sys_error _ -> print_endline "Error: File not found"
  | _ -> print_endline "Error: Failed to load and parse ASCII CSV."

(* Test case for display_pokemon function *)

let test_display_learnable_moves _ = display_learnable_moves "charizard"

(* Test valid Pokémon ID *)
let test_display_pokemon _ =
  let valid_id = 1 in
  let invalid_id = 9999 in

  (* Test valid Pokémon ID *)
  try
    display_pokemon valid_id;
    print_endline "test_display_pokemon passed for valid ID."
  with
  | Failure _ -> print_endline "Error: Unexpected failure for valid ID."
  | _ -> (
      print_endline "Error: Failed to display valid Pokémon.";

      (* Test invalid Pokémon ID *)
      try
        display_pokemon invalid_id;
        print_endline
          "Error: Expected failure for invalid Pokémon ID but got success."
      with
      | Failure _ -> print_endline "test_display_pokemon passed for invalid ID."
      | _ ->
          print_endline "Error: Unexpected error handling invalid Pokémon ID.")

(* let test_apply_effect () = (* Mocked attacker and defender with initial
   values *) let attacker = dual_pokemon in

   let defender = non_dual_pokemon in let move_with_effect effect_id
   effect_chance target = { id = 0; name = "TestMove"; tipe = "Normal";
   effect_id; effect_chance; target; power = 50; accuracy = 100; priority = 0;
   damage_class = Physical; pp = 10; } in

   (* Test cases *)

   (* Test status ailment effects *) let move_sleep = move_with_effect 2 100
   Self in let a, d = apply_effect attacker defender move_sleep in assert
   (d.ailment = "asleep");

   (* Attacker should fall asleep *) let move_poison = move_with_effect 3 100
   Self in let a, d = apply_effect attacker defender move_poison in assert
   (d.ailment = "poisoned");

   (* Attacker should be poisoned *) let move_burn = move_with_effect 5 100 Self
   in let a, d = apply_effect attacker defender move_burn in assert (d.ailment =
   "burned");

   (* Attacker should be burned *)

   (* Test stat stage effects *) let move_raise_attack = move_with_effect 11 100
   Self in let a, d = apply_effect attacker defender move_raise_attack in assert
   (atk d = 1);

   (* Attacker's attack should increase by 1 stage *) let move_lower_attack =
   move_with_effect 19 100 Self in let a, d = apply_effect attacker defender
   move_lower_attack in assert (atk d = -1);

   (* Attacker's attack should decrease by 1 stage *) let move_reset_stats =
   move_with_effect 26 100 Self in let a, d = apply_effect attacker defender
   move_reset_stats in assert (d.stat_stages = zero_stats);

   (* Attacker's stats should be reset *)

   (* Test damage effects *) let move_flat_damage = move_with_effect 42 100 Self
   in let a, d = apply_effect attacker defender move_flat_damage in assert_equal
   0 (d.cur_hp) ~cmp:(fun _ y -> y <= 60 && y >= 0);

   (* Defender should lose 40 HP *) let move_ohko = move_with_effect 39 100 Self
   in let a, d = apply_effect attacker defender move_ohko in assert_equal 1 1;

   (* Defender should be knocked out *)

   (* Test healing effects *) let move_heal_half_hp = move_with_effect 33 100
   Self in let a, d = apply_effect attacker defender move_heal_half_hp in
   assert_equal 1 1;

   (* Attacker should heal half HP *) let move_heal_full_and_sleep =
   move_with_effect 38 100 Self in let a, d = apply_effect attacker defender
   move_heal_full_and_sleep in assert_equal 1 1;

   (* Attacker should heal fully and fall asleep *)

   (* Test effect based on chance *) let move_with_effect_chance =
   move_with_effect 19 50 Self in let a, d = apply_effect attacker defender
   move_with_effect_chance in (* Attack stat may or may not change based on the
   roll (50% chance) *) assert_equal atk d 0;

   (* Could lower attack or leave it unchanged *)

   (* Test self-targeting move *) let move_self_target = move_with_effect 2 100
   Self in let a, d = apply_effect attacker defender move_self_target in
   assert_equal a.ailment "asleep";

   (* Attacker should fall asleep *)

   (* Test recoil *) let move_recoil = move_with_effect 199 100 Self in let a, d
   = apply_effect attacker defender move_recoil in assert_equal a.cur_hp 90;

   (* Attacker should lose 10% of their HP *)

   (* Test type copying *) let move_copy_type = move_with_effect 31 100 Self in
   let a, d = apply_effect attacker defender move_copy_type in assert_equal
   d.tipe ("Water", "NoneType");

   (* Defender should copy attacker's type *)

   (* Test multiple effects (self-targeting and damage) *) let
   move_multiple_effects = move_with_effect 42 100 Self in let a, d =
   apply_effect attacker defender move_multiple_effects in assert_equal d.cur_hp
   60;

   (* Defender should take damage, attacker should recoil *) Printf.printf "All
   tests passed!\n" ;;

   test_apply_effect () *)

let tests =
  "test suite"
  >::: List.flatten
         [ pokemon_stats_test_cases; stat_change_tests; other_pokemon_tests ]
       @ [
           "test_chunk_string" >:: test_chunk_string;
           "test_load_ascii_csv" >:: test_load_ascii_csv;
           "test_display_pokemon" >:: test_display_pokemon;
           "test_display_learnable_moves" >:: test_display_learnable_moves;
         ]

let _ = run_test_tt_main tests
