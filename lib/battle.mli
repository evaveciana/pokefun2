
type team
(** type for a team of pokemon *)
type decision
(** variant for a decision that a player can make *)

type battle_state
and battle_status
(** type for the current state of the battle *)




val init_battle : team -> team -> battle_state
(** starts the battle by having two teams start the game with populated moves and full health*)
val create_random_team : unit -> team
(** creates a random team of 6 pokemon, 4 moves each, random nature*)
val create_ai_team : unit -> team
(** creates reasonably balanced team of 6 pokemon, 4 moves each, random natures*)
val get_player_action : unit -> decision
(** waits for the player action to input something in the terminal (currently no gui yet)*)

val make_ai_action : battle_state -> decision
(** take the current state and make a new*)
val handle_player_decision : decision -> battle_state -> battle_state
(** takes decision and advances the current battle_state further to progress the game*)
(* val ai_action : battle_state -> battle_state *)

val battle_loop : battle_state -> unit 
(** loops through the battle until the state reaches ended *)

val pick_team : string list -> team
(** waits for player input to pick a team*)
val main_menu : unit -> unit
(** begins the game by allowing players to select which pokemon they want *)
