type stats
(** type for the stats of a Pokemon *)

type damage_class
(** type for a Pokemon's damage class *)

type target
(** type for the target of a Pokemon's move *)

type move = {
  id : int;
  name : string;
  tipe : string;
  power : int;
  pp : int;
  accuracy : int;
  priority : int;
  target : target;
  damage_class : damage_class;
  effect_id : int;
  effect_chance : int;
}
(** type for a Pokemon attack (move)*)

(* type ailment *)

(** type for ailment *)
val valid_ailments : string list
(** all the acceptable ailments of a Pokemon *)

type t = {
  species : string;
  is_dual_type : bool;
  tipe : string * string;
  base_stats : stats;
  cur_stats : stats;
  stat_stages : stats;
  moves : move list;
  level : int;
  ailment : string;
  nature : string;
  mutable cur_hp : int;
}
(** type for a Pokemon*)

exception BadPokemon
(** Raised when a user attempts to create a Pokemon with invalid inputs. *)

val zero_stats : stats
(** [zero_stats] is a collection of stats with every stat initialized to zero. *)

val one_stats : stats
(** [one_stats] is a collection of stats with every stat initialized to zero. *)

(* val species : t -> string *)
(** [species p] is the species of pokemon [p] *)

(* val base_stats : t -> stats *)
(** [base_stats p] is the base stats of pokemon [p] *)

val cur_stats : t -> stats
(** [cur_stats p] is the current stats of pokemon [p], including any changes
    from burn, paralysis, etc. *)

(* val base_hp : t -> int *)
(** [base_hp p] is the base HP of pokemon [p]. *)

(* val base_atk : t -> int *)
(** [base_atk p] is the base Attack of pokemon [p]. *)

(* val base_spatk : t -> int *)
(** [base_spatk p] is the base Special Attack of pokemon [p]. *)

(* val base_def : t -> int *)
(** [base_def p] is the base Defense of pokemon [p]. *)

(* val base_spdef : t -> int *)
(** [base_spdef p] is the base Special Defense of pokemon [p]. *)

(* val base_spd : t -> int *)
(** [base_spd p] is the base Speed of pokemon [p]. *)

val cur_hp : t -> int
(** [cur_hp p] is the current HP of pokemon [p], including changes from damage. *)

val max_hp : t -> int
(** [max_hp p] is the max HP of pokemon [p] *)

val atk : t -> int
(** [atk p] is the current Attack of pokemon [p], including changes from stat
    stages. *)

val spatk : t -> int
(** [spatk p] is the current Special Attack of pokemon [p], including changes
    from stat stages. *)

val def : t -> int
(** [def p] is the current Defense of pokemon [p], including changes from stat
    stages. *)

val spdef : t -> int
(** [spdef p] is the current Special Defense of pokemon [p], including changes
    from stat stages. *)

val spd : t -> int
(** [spd p] is the current Speed of pokemon [p], including changes from stat
    stages. *)

val moves : t -> move list
(** [moves p] is the list of moves currently added to [p]. *)

val attack : t -> t -> move -> t * t
(** [attack attacker defender move] Causes pokemon [attacker] to use [move] on
    pokemon [defender] and returns the resulting (attacker, defender) as a tuple *)

val update_current_stats : t -> t
(** [update_current_stats p] updates the current stats of a pokemon with its
    current status.*)

val stats_to_list : stats -> int list
(** [stats_to_list stats] is a list representation of [stats]. *)

val get_move_id_from_name : string -> int
(** [get_move_id_from_name str] is the csv move id of the move of name 'str'.*)

val get_move_id_from_move : move -> int
(** [get_move_id_from_move move] is the move id of the move [move]*)

val move_to_string : move -> string
(** [move_to_string move] is the necessary information in string form of move*)

val pokemon_to_string : t -> string
(** [pokemon_to_string t] is the name and learned moves of a pokemon*)

val create_move_from_name : string -> move
(** [create_move_from_name str] from the csv, initializes a move record with
    name 'str'.*)

val display_learnable_moves : string -> unit
(** [display_learnable_moves str] prints out each move that the pokemon of
    species str can learn, with some basic info about the move. *)

val get_moves : string -> string list
(** [get_moves str] is the list of moves associated with a Pokemon of species
    [str].*)

val create : string -> int -> string -> t
(** [create species level nature] is a [species] pokemon that is level [level],
    and has nature [nature].*)

val calc_effectiveness_mult : move -> t -> float
(** [calc_effectiveness_mult move t] is the float multiplier that is used to
    calculate total damage done from a move*)

val attack : t -> t -> move -> t * t
(** [attack a d move] is a pair of Pokemon. The first element is identical to
    the attacker [a], but with updated stats after making move [move], and the
    second element is identical to the the defender [d], but with updated stats
    after [a] made move [move]. *)

val apply_stat_change : t -> string -> int -> t
(** [apply_stat_change p stat_name num_stages] applies [num_stages] stat change
    to the stat [stat_name] of pokemon [p] and returns the new pokemon.*)

val add_pokemon_move : t -> string -> t
(** [add_pokemon_move pokemon move_name] adds the move with name [move_name] to
    [pokemon]'s list of moves and returns an updated Pokémon with the new move.
    Raises [failure] if the Pokémon already has 4 moves. *)

val get_pokemon_id : string -> int
(**[get_pokemon_id poke_name] is the ID of the Pokémon with the name
   [poke_name]. Searches for [poke_name] in the "data/pokemon.csv" file,
   retrieves the corresponding ID, and returns it as an integer. Raises
   [failure] if the Pokémon name is not found or the data is invalid. *)

val calc_damage : t -> t -> move -> int
(** [calc_damage attacker defender move] is how much damage that move would do
    by the attacker to the defender *)
