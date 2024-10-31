type stats
(** type for the stats of a Pokemon *)

type tipe
(** type for a Pokemon type *)

(* type nature *)
(** type for a Pokemon's nature *)

val valid_natures : string list
(** all the acceptable natures of a Pokmeon *)

type damage_class
(** type for a Pokemon's damage class *)

type target
(** type for the target of a Pokemon's move *)

type move
(** type for a Pokemon attack (move)*)

(* type ailment *)

(** type for ailment *)
val valid_ailments : string list
(** all the acceptable ailments of a Pokemon *)

type t
(** type for a Pokemon*)

exception BadPokemon
(** Raised when a user attempts to create a Pokemon with invalid inputs. *)

val zero_stats : stats
(** A collection of stats with every stat initialized to zero. *)

(* val basic_move : move *)
(**Exposed for testing purposes*)

(* val basic_tipe : tipe * tipe *)
(**Exposed for testing purposes*)

(* val basic_nature : nature *)
(**Exposed for testing purposes*)

val species : t -> string
(** [species p] is the species of pokemon [p] *)

val base_stats : t -> stats
(** [base_stats p] is the base stats of pokemon [p] *)

val cur_stats : t -> stats
(** [cur_stats p] is the current stats of pokemon [p], including any changes
    from burn, paralysis, etc. *)

val base_hp : t -> int
(** [base_hp p] is the base HP of pokemon [p]. *)

val base_atk : t -> int
(** [base_atk p] is the base Attack of pokemon [p]. *)

val base_spatk : t -> int
(** [base_spatk p] is the base Special Attack of pokemon [p]. *)

val base_def : t -> int
(** [base_def p] is the base Defense of pokemon [p]. *)

val base_spdef : t -> int
(** [base_spdef p] is the base Special Defense of pokemon [p]. *)

val base_spd : t -> int
(** [base_spd p] is the base Speed of pokemon [p]. *)

val hp : t -> int
(** [hp p] is the current HP of pokemon [p], including changes from damage. *)

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

(*Need acc and eva?*)

val attack : t -> t -> move -> t * t
(** [attack attacker defender move] Causes pokemon [attacker] to use [move] on
    pokemon [defender] and returns the resulting (attacker, defender) as a tuple *)

val calc_current_stats : stats -> string -> int -> string -> stats -> stats
(** [calc_current_stats base_stats nature level ailment stat_stages] returns the
    current stats of a pokemon with base stats [base_stats], nature [nature],
    level [level], ailment [ailment], with stat stages [stat_stages] Should be
    called when initializing a pokemon, healing, or leveling up *)

val stats_to_list : stats -> int list
(** [stats_to_list stats] is a list representation of [stats]. *)

val move_ids : move list -> int list
(** [move_ids lst] is the list of integer ids representing the moves in [lst]. *)

val get_moves : string -> move list
(** [get_moves str] is the list of moves associated with a Pokemon of species
    [str]. *)

val create : string -> int -> string -> t
(** [create species level nature] is a [species] pokemon that is level [level],
    and has nature [nature] *)

val attack : t -> t -> move -> t * t
(** [attack a d move] is a pair of Pokemon. The first element is identical to
    the attacker [a], but with updated stats after making move [move], and the
    second element is identical to the the defender [d], but with updated stats
    after [a] made move [move]. *)

val apply_stat_change : t -> string -> int -> t
(** [apply_stat_change p stat_name num_stages] applies [num_stages] stat change
    to the stat [stat_name] of pokemon [p] and returns the new pokemon *)

val add_pokemon_move : t -> int -> t
(** [add_pokemon_move pokemon new_move_id] adds the move with id [new_move_id]
    to [pokemon]'s list of moves and returns an updated Pokémon with the new
    move. Raises [failure] if the Pokémon already has 4 moves. *)
