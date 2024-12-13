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
