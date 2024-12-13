
type team = Pokemon.t list
(** type for a team of pokemon *)
type decision =
  | Attack
  | Switch
  | Run

(** variant for a decision that a player can make *)
type t = Pokemon.t

type battle_state = {
  team1 : team;
  team2 : team;
  current_turn : int;
  status : battle_status;
  current_pokemon : Pokemon.t * Pokemon.t;
}
and battle_status =
  | PlayerTurn
  | Team1Win
  | Team2Win
(** type for the current state of the battle *)


val setup_fake : unit -> battle_state

val create_player_teams : unit -> team

val init_battle : team -> team -> battle_state
(** starts the battle by having two teams start the game with populated moves and full health*)

val create_ai_team : unit -> team
(** creates reasonably balanced team of 6 pokemon, 4 moves each, random natures*)
val get_player_action : unit -> decision
(** waits for the player action to input something in the terminal (currently no gui yet)*)

(* val make_ai_action : battle_state -> decision *)
(** take the current state and make a new*)
(* val handle_player_decision : decision -> battle_state -> battle_state *)
(** takes decision and advances the current battle_state further to progress the game*)
(* val ai_action : battle_state -> battle_state *)
val handle_action : decision -> Pokemon.move -> battle_state -> battle_state

val battle_loop : battle_state -> unit 
(** loops through the battle until the state reaches ended *)

val pick_team : string list -> team
(** waits for player input to pick a team*)
val main_menu : unit -> unit
(** begins the game by allowing players to select which pokemon they want *)
val attack_menu : battle_state -> Pokemon.move

val add_moves : t -> string list -> t

val switch_menu : battle_state -> t

val get_ai_move : battle_state -> Pokemon.move

val win : unit -> unit
val lose : unit -> unit
