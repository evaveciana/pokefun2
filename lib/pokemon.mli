type stats
(** type for the stats of a Pokemon *)

type tipe
(** type for a Pokemon type *)

(* type nature *)
(** type for a Pokemon's nature *)

type move
(** type for a Pokemon attack (move)*)

type ailment
(** type for ailment *)

type t
(** type of a Pokemon*)

exception BadPokemon
(**Raised when a user attempts to create a Pokemon with invalid inputs.*)

val zero_stats : stats
(**Exposed for testing purposes*)

val basic_move : move
(**Exposed for testing purposes*)

val basic_tipe : tipe * tipe
(**Exposed for testing purposes*)

(* val basic_nature : nature *)
(**Exposed for testing purposes*)

val create : string -> move list -> int -> string -> t
(** [create species move_list level nature] is a [species] pokemon that is level
    [level], has moves [move_list], and nature [nature]*)

val species : t -> string
(** [species p] is the species of pokemon [p]*)

val base_stats : t -> stats
(** [base_stats p] is the base stats of pokemon [p] *)

val cur_stats : t -> stats
(** [cur_stats p] is the current stats of pokemon [p], including any changes
    from burn, paralysis, etc.*)

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
(** [max_hp p] is the max HP of pokemon [p]*)

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

val attack : t -> t -> move -> t * t
(** [attack attacker defender move] Causes pokemon [attacker] to use [move] on
    pokemon [defender] and returns the resulting (attacker, defender) as a tuple *)

val apply_status_effect : t -> string -> int -> t
(** [apply_status_effect p stat_name num_stages] applies [num_stages] stat
    change to the stat [stat_name] of pokemon [p] and returns the new pokemon *)

val calc_current_stats : stats -> string -> int -> ailment -> stats -> stats
(** [calc_current_stats base_stats nature level stat_stages] returns the current
    stats of a pokemon with base stats [base_stats], nature [nature], level
    [level], with stat stages [stat_stages] Should be called when initializing a
    pokemon, healing, or leveling up*)

val add_pokemon_move : t -> move -> t
(** [add_pokemon_move pokemon new_move] adds [new_move] to [pokemon]'s list of
    moves and returns an updated Pokémon with the new move. Raises [failure] if
    the Pokémon already has 4 moves. *)
