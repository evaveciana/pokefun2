(* Define a type for Pokemon stats *)
type stats = {
  hp : int;
  attack : int;
  defense : int;
  speed : int;
}

(* Define a variant type for species *)
type species =
  | Pikachu
  | Charmander
  | Bulbasaur
  | Squirtle

type tipe =
  | Water
  | Fire
  | Grass
  | Electric

type move = {
  name : string;
  power : int;
  move_type : tipe;
}

module type POKEMON = sig
  type t
  (** type of a pokemon*)

  val create : string -> int -> t
  (** [create species level] is a [species] pokemon that is level [level] *)

  val species : t -> species
  (** [species p] is the species of pokemon [p]*)

  val base_stats : t -> stats
  (** [base_stats p] is the base stats of pokemon [p]*)

  val cur_stats : t -> stats
  (** [cur_stats p] is the current stats of pokemon [p]*)

  val attack : t -> t -> move -> t
  (** [attack attacker defender move] Causes pokemon [attacker] to use [move] on
      pokemon [defender] and returns the resulting defending pokemon *)
end

module Pokemon = struct
  (* type t = { species : species; base_stats : stats; cur_stats : stats; } *)

  type t = {
    species : string;
    tipe : tipe;
    base_stats : int;
    cur_stats : int;
    moves : move list;
  }

  let create name level =
    { species = name; tipe = Water; base_stats = 0; cur_stats = 0; moves = [] }

  let attack attacker defender = 0
end

let team = [ Pokemon.create "" 1; Pokemon.create "" 1; Pokemon.create "" 1 ]
let temp = Pokemon.create "" 1
