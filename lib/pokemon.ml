(* Define a type for Pokemon stats *)
type stats = {
  hp : int;
  attack : int;
  defense : int;
  speed : int;
}

type move =
  | Tackle
  | Growl
  | Ember

(* Define a variant type for species *)
type species =
  | Pikachu
  | Charmander
  | Bulbasaur
  | Squirtle

module type POKEMON = sig
  type t

  (* val calculate_stats : stats -> stats *)
end

module Pokemon = struct
  (* type t = { species : species; base_stats : stats; cur_stats : stats; } *)

  type t = {
    species : string;
    base_stats : int;
    cur_stats : int;
  }

  let create name level = { species = name; base_stats = 0; cur_stats = 0 }
  let attack attacker defender = 0
end

let team = [ Pokemon.create "" 1; Pokemon.create "" 1; Pokemon.create "" 1 ]
let temp = Pokemon.create "" 1
