type stats = {
  hp : int;
  atk : int;
  spatk : int;
  def : int;
  spdef : int;
  spd : int;
}

type tipe =
  | Water
  | Fire
  | Grass
  | Electric
  | Fighting
  | Flying
  | Psychic
  | Ghost
  | Normal
  | Fairy
  | Steel
  | Ice
  | Dark
  | Dragon
  | Bug
  | Rock
  | Ground
  | Poison
  | None

type nature =
  | Hardy
  | Lonely
  | Brave
  | Adamant
  | Naughty
  | Bold
  | Docile
  | Relaxed
  | Impish
  | Lax
  | Timid
  | Hasty
  | Serious
  | Jolly
  | Naive
  | Modest
  | Mild
  | Quiet
  | Bashful
  | Rash
  | Calm
  | Gentle
  | Sassy
  | Careful
  | Quirky

type move = {
  name : string;
  power : int;
  move_type : tipe;
  category : string;
  pp : int;
}

type ailment =
  | Burned
  | Paralyzed
  | Asleep
  | Frozen
  | Confused
  | Healthy

type t = {
  species : string;
  is_dual_type : bool;
  tipe : tipe * tipe;
  base_stats : stats;
  cur_stats : stats;
  stat_stages : stats;
  moves : move list;
  level : int;
  ailment : ailment;
}

let zero_stats = { hp = 0; atk = 0; spatk = 0; def = 0; spdef = 0; spd = 0 }

let create name level =
  {
    species = name;
    is_dual_type = false;
    tipe = (Water, None);
    base_stats = zero_stats;
    cur_stats = zero_stats;
    stat_stages = zero_stats;
    moves = [];
    level;
    ailment = Healthy;
  }

let species p = p.species
let base_stats p = p.base_stats
let cur_stats p = p.cur_stats (* Modify with ailment and stat stages *)
let base_hp p = (base_stats p).hp
let base_atk p = (base_stats p).atk
let base_spatk p = (base_stats p).spatk
let base_def p = (base_stats p).def
let base_spdef p = (base_stats p).spdef
let base_spd p = (base_stats p).spd
let hp p = (cur_stats p).hp
let max_hp p = 0 (* TODO*)
let atk p = (cur_stats p).atk
let spatk p = (cur_stats p).spatk
let def p = (cur_stats p).def
let spdef p = (cur_stats p).spdef
let spd p = (cur_stats p).spd
let attack attacker defender move = create "" 1
let apply_status_effect p stat_name num_stages = create "" 1

let get_tipe_from_species species =
  match species with
  | "Pikachu" -> (Electric, None)
  | _ -> (None, None)

let get_stats_from_species species = zero_stats
let calc_stats base_stats nature level = zero_stats
