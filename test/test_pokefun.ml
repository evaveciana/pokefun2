open OUnit2
open Pokefun.Pokemon

let make_stats_test (name, f, input, expected) =
  name >:: fun _ -> assert_equal expected (f input) ~printer:string_of_int

let dual_pokemon = create "bulbasaur" 1 "hardy"
let non_dual_pokemon = create "charmander" 5 "quiet"

let pokemon_stats_tests =
  [
    ("Base HP", base_hp, dual_pokemon, 45);
    ("Base attack", base_atk, dual_pokemon, 49);
    ("Base special attack", base_spatk, dual_pokemon, 65);
    ("Base defense", base_def, dual_pokemon, 49);
    ("Base special defense", base_spdef, dual_pokemon, 65);
    ("Base speed", base_spd, dual_pokemon, 45);
    ("HP", hp, dual_pokemon, 12);
    ("Max hp", max_hp, dual_pokemon, 0);
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
      assert_raises BadPokemon (fun () -> create "eva" 1 "hardy") );
    ( "Bad create 2" >:: fun _ ->
      assert_raises BadPokemon (fun () -> create "Bulbasaur" 0 "hardy") );
    ( "Bad create 3" >:: fun _ ->
      assert_raises BadPokemon (fun () -> create "Bulbasaur" 1 "confused") );
    ( "Species 1" >:: fun _ ->
      assert_equal "bulbasaur" (species dual_pokemon) ~printer:Fun.id );
    ( "Species 2" >:: fun _ ->
      assert_equal "charmander" (species non_dual_pokemon) ~printer:Fun.id );
    ( "Base stats" >:: fun _ ->
      assert_equal
        [ 45; 49; 49; 65; 65; 45; 0; 0 ]
        (stats_to_list (base_stats dual_pokemon))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Current stats/calc current stats" >:: fun _ ->
      assert_equal
        [ 12; 6; 6; 6; 6; 6; 6; 6 ] (*Not working-- acc and eva should be 100?*)
        (stats_to_list (cur_stats dual_pokemon))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Attack" >:: fun _ ->
      assert_equal
        (dual_pokemon, dual_pokemon)
        (attack dual_pokemon dual_pokemon (List.hd (get_moves "Bulbasaur"))) );
    ( "Apply stat change" >:: fun _ ->
      assert_equal
        [ 12; 6; 6; 6; 6; 6; 6; 6 ]
        (stats_to_list (cur_stats (apply_stat_change dual_pokemon "hp" 0)))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Add move" >:: fun _ ->
      assert_equal [ 14; 75; 33 ]
        (move_ids
           (moves
              (add_pokemon_move
                 (add_pokemon_move (add_pokemon_move dual_pokemon 14) 75)
                 33)))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Bad add move 1" >:: fun _ ->
      assert_raises (Failure "Unrecognized move id") (fun () ->
          add_pokemon_move dual_pokemon 1000) );
    ( "Bad add move 2" >:: fun _ ->
      assert_raises (Failure "A Pokémon can only have up to 4 moves.")
        (fun () ->
          add_pokemon_move
            (add_pokemon_move
               (add_pokemon_move
                  (add_pokemon_move (add_pokemon_move dual_pokemon 14) 75)
                  33)
               22)
            77) );
    ( "Bad add move 3" >:: fun _ ->
      assert_raises (Failure "This move is not permitted by this species")
        (fun () -> add_pokemon_move dual_pokemon 58) );
    ( "Get move ID from name" >:: fun _ ->
      assert_equal 24
        (get_move_id_from_name "double-kick")
        ~printer:string_of_int );
    ( "Move to string" >:: fun _ ->
      assert_equal
        "Vine Whip (Type: Grass, Power:\n\
        \     45, PP: 25, Accuracy: 100, Damage Class: Physical)"
        (move_to_string (example_move ()))
        ~printer:Fun.id );
    ( "Pokemon to string" >:: fun _ ->
      assert_equal
        "Bulbasaur: Swords Dance (Type: Normal, Power: , PP: 20, Accuracy: 100, \n\
        \        Damage Class: Status)"
        (pokemon_to_string (add_pokemon_move dual_pokemon 14))
        ~printer:Fun.id );
    ( "Create move from name" >:: fun _ ->
      assert_equal (example_move ()) (create_move_from_name "vine-whip") );
  ]

let tests =
  "test suite"
  >::: List.flatten [ pokemon_stats_test_cases; other_pokemon_tests ]

let _ = run_test_tt_main tests
