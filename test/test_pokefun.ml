open OUnit2
open Pokefun.Pokemon

let make_stats_test (name, f, input, expected) =
  name >:: fun _ -> assert_equal expected (f input) ~printer:string_of_int

let sample_pokemon = create "Bulbasaur" 1 "hardy"

let pokemon_stats_tests =
  [
    ("Base HP", base_hp, sample_pokemon, 45);
    ("Base attack", base_atk, sample_pokemon, 49);
    ("Base special attack", base_spatk, sample_pokemon, 65);
    ("Base defense", base_def, sample_pokemon, 49);
    ("Base special defense", base_spdef, sample_pokemon, 65);
    ("Base speed", base_spd, sample_pokemon, 45);
    ("HP", hp, sample_pokemon, 12);
    ("Max hp", max_hp, sample_pokemon, 0);
    ("Attack", atk, sample_pokemon, 6);
    ("Special attack", spatk, sample_pokemon, 6);
    ("Defense", def, sample_pokemon, 6);
    ("Special defense", spdef, sample_pokemon, 6);
    ("Speed", spd, sample_pokemon, 6);
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
    ( "Species" >:: fun _ ->
      assert_equal "bulbasaur" (species sample_pokemon) ~printer:Fun.id );
    ( "Base stats" >:: fun _ ->
      assert_equal
        [ 45; 49; 49; 65; 65; 45; 0; 0 ]
        (stats_to_list (base_stats sample_pokemon))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Current stats/calc current stats" >:: fun _ ->
      assert_equal
        [ 12; 6; 6; 6; 6; 6; 6; 6 ] (*Not working-- acc and eva should be 100?*)
        (stats_to_list (cur_stats sample_pokemon))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Attack" >:: fun _ ->
      assert_equal
        (sample_pokemon, sample_pokemon)
        (attack sample_pokemon sample_pokemon (List.hd (get_moves "Bulbasaur")))
    );
    ( "Apply status effects" >:: fun _ ->
      assert_equal [ 1; 6; 6; 6; 6; 6; 6; 6 ]
        (stats_to_list (cur_stats (apply_status_effect sample_pokemon "hp" 0)))
    );
    ( "Add move" >:: fun _ ->
      assert_equal [ 14; 75; 33 ]
        (move_ids
           (moves
              (add_pokemon_move
                 (add_pokemon_move (add_pokemon_move sample_pokemon 14) 75)
                 33)))
        ~printer:(fun x -> String.concat ", " (List.map string_of_int x)) );
    ( "Bad add move 1" >:: fun _ ->
      assert_raises (Failure "Unrecognized move id") (fun () ->
          add_pokemon_move sample_pokemon 1000) );
    ( "Bad add move 2" >:: fun _ ->
      assert_raises (Failure "A Pokémon can only have up to 4 moves.")
        (fun () ->
          add_pokemon_move
            (add_pokemon_move
               (add_pokemon_move
                  (add_pokemon_move (add_pokemon_move sample_pokemon 14) 75)
                  33)
               22)
            77) );
    ( "Bad add move 3" >:: fun _ ->
      assert_raises (Failure "This move is not permitted by this species")
        (fun () -> add_pokemon_move sample_pokemon 58) );
  ]

let tests =
  "test suite"
  >::: List.flatten [ pokemon_stats_test_cases; other_pokemon_tests ]

let _ = run_test_tt_main tests
