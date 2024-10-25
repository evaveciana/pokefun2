type stats
(** type for the stats of a Pokemon *)

type tipe
(** type for a Pokemon type *)

type nature
(** type for a Pokemon's nature *)

type move
(** type for a Pokemon attack (move)*)

type t
(** type of a Pokemon*)

val create : string -> int -> t
(** [create species level] is a [species] pokemon that is level [level] *)

val species : t -> string
(** [species p] is the species of pokemon [p]*)

val base_stats : t -> stats
(** [base_stats p] is the base stats of pokemon [p]*)

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

val attack : t -> t -> move -> t
(** [attack attacker defender move] Causes pokemon [attacker] to use [move] on
    pokemon [defender] and returns the resulting defending pokemon *)

val get_tipe_from_species : string -> tipe * tipe
(** [get_tipe_from_species spec] returns the tipes of a [spec] pokemon *)

val get_stats_from_species : string -> stats
(** [get_stats_from_species spec] returns the stats of a [spec] pokemon *)

val calc_stats : stats -> nature -> int -> stats
(** [calc_stats base_stats nature level] returns the current stats of a pokemon
    with base stats [base_stats], nature [nature], and level [level] Should be
    called when initializing a pokemon, healing, or leveling up*)
