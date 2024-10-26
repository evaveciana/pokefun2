open OUnit2
open Pokefun.Pokemon

let make_stats_test (name, f, input, expected) =
  name >:: fun _ -> assert_equal expected (f input)

let sample_pokemon = create "Bulbasaur" [] 1 "hardy"

let pokemon_stats_tests =
  [
    ("Base HP", base_hp, sample_pokemon, 0);
    ("Base attack", base_atk, sample_pokemon, 0);
    ("Base special attack", base_spatk, sample_pokemon, 0);
    ("Base defense", base_def, sample_pokemon, 0);
    ("Base special defense", base_spdef, sample_pokemon, 0);
    ("Base speed", base_spd, sample_pokemon, 0);
    ("HP", hp, sample_pokemon, 0);
    ("Max hp", max_hp, sample_pokemon, 0);
    ("Attack", atk, sample_pokemon, 0);
    ("Special attack", spatk, sample_pokemon, 0);
    ("Defense", def, sample_pokemon, 0);
    ("Special defense", spdef, sample_pokemon, 0);
    ("Speed", spd, sample_pokemon, 0);
  ]

let pokemon_stats_test_cases = List.map make_stats_test pokemon_stats_tests

let other_pokemon_tests =
  [
    ("Species" >:: fun _ -> assert_equal "Bulbasaur" (species sample_pokemon));
    ( "Base stats" >:: fun _ ->
      assert_equal zero_stats (base_stats sample_pokemon) );
    ( "Current stats" >:: fun _ ->
      assert_equal zero_stats (cur_stats sample_pokemon) );
    ( "Attack" >:: fun _ ->
      assert_equal
        (sample_pokemon, sample_pokemon)
        (attack sample_pokemon sample_pokemon basic_move) );
    ( "Apply status effects" >:: fun _ ->
      assert_equal sample_pokemon (apply_status_effect sample_pokemon "hp" 0) );
    (* ( "Calculate stats" >:: fun _ -> assert_equal zero_stats (calc_stats
       zero_stats "hardy" 1) ); *)
  ]

let tests =
  "test suite"
  >::: List.flatten [ pokemon_stats_test_cases; other_pokemon_tests ]

let _ = run_test_tt_main tests

(*Idea: make a way to encode a move as a hashcode or something to use in tests*)
