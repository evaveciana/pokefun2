(* Type definitions *)
type team = Pokemon.t array

(* Type for a team of Pokémon *)
type decision =
  | Attack of string
  | Switch of string
  | UseItem of string
  | Run

(* Type for the decision that a player can make *)

type battle_state = {
  team1 : team;
  team2 : team;
  current_turn : int;
  status : battle_status;
}

and battle_status =
  | Idle
  | PlayerTurn
  | AITurn
  | Ended

(* Type for the current state of the battle *)

(* Functions *)
let init_battle (team1 : team) (team2 : team) : battle_state =
  { team1; team2; current_turn = 0; status = Idle }

let create_random_team () : team = failwith "TODO"
let create_ai_team () : team = failwith "TODO"
let get_player_action () : decision = Run (* Placeholder implementation *)

let make_ai_action (battle : battle_state) : decision =
  Run (* Placeholder implementation *)

let handle_player_decision (decision : decision) (battle : battle_state) :
    battle_state =
  { battle with current_turn = battle.current_turn + 1 }
