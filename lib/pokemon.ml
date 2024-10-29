type stats = {
  hp : int;
  atk : int;
  def : int;
  spatk : int;
  spdef : int;
  spd : int;
  acc : int;
  eva : int;
}

type tipe =
  | Normal
  | Fire
  | Water
  | Grass
  | Electric
  | Ice
  | Fighting
  | Poison
  | Ground
  | Flying
  | Psychic
  | Bug
  | Rock
  | Ghost
  | Dark
  | Dragon
  | Steel
  | Fairy
  | NoneType

(* type nature = | Hardy | Lonely | Brave | Adamant | Naughty | Bold | Docile |
   Relaxed | Impish | Lax | Timid | Hasty | Serious | Jolly | Naive | Modest |
   Mild | Quiet | Bashful | Rash | Calm | Gentle | Sassy | Careful | Quirky *)

let valid_natures =
  [
    "hardy";
    "lonely";
    "brave";
    "adamant";
    "naughty";
    "bold";
    "docile";
    "relaxed";
    "impish";
    "lax";
    "timid";
    "hasty";
    "serious";
    "jolly";
    "naive";
    "modest";
    "mild";
    "quiet";
    "bashful";
    "rash";
    "calm";
    "gentle";
    "sassy";
    "careful";
    "quirky";
  ]

type damage_class =
  | Status
  | Physical
  | Special

type target =
  | Self
  | Enemy

type move = {
  id : int;
  name : string;
  tipe : tipe;
  power : int;
  pp : int;
  accuracy : int;
  priority : int;
  target : target;
  damage_class : damage_class;
  effect_id : int;
  effect_chance : int;
}

(* type ailment = | Burned | Paralyzed | Asleep | Frozen | Confused | Healthy *)

let valid_ailments =
  [ "burned"; "paralyzed"; "asleep"; "frozen"; "confused"; "healthy" ]

type t = {
  species : string;
  is_dual_type : bool;
  tipe : tipe * tipe;
  base_stats : stats;
  cur_stats : stats;
  stat_stages : stats;
  moves : move list;
  level : int;
  ailment : string;
  nature : string;
  cur_hp : int;
}

exception BadPokemon

let zero_stats =
  { hp = 0; atk = 0; def = 0; spatk = 0; spdef = 0; spd = 0; acc = 0; eva = 0 }

let species p = p.species
let base_stats p = p.base_stats
let cur_stats p = p.cur_stats (* Modify with ailment and stat stages *)
let base_hp p = p.base_stats.hp
let base_atk p = p.base_stats.atk
let base_spatk p = p.base_stats.spatk
let base_def p = p.base_stats.def
let base_spdef p = p.base_stats.spdef
let base_spd p = p.base_stats.spd
let hp p = p.cur_stats.hp
let max_hp p = 0 (* TODO*)
let atk p = p.cur_stats.atk
let def p = p.cur_stats.def
let spatk p = p.cur_stats.spatk
let spdef p = p.cur_stats.spdef
let spd p = p.cur_stats.spd
let moves p = p.moves

let get_multipliers_by_nature n =
  match n with
  | "lonely" -> (1.0, 1.1, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "adamant" -> (1.0, 1.1, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0)
  | "naughty" -> (1.0, 1.1, 1.0, 1.0, 0.9, 1.0, 1.0, 1.0)
  | "brave" -> (1.0, 1.1, 1.0, 1.0, 1.0, 0.9, 1.0, 1.0)
  | "bold" -> (1.0, 0.9, 1.1, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "impish" -> (1.0, 1.0, 1.1, 0.9, 1.0, 1.0, 1.0, 1.0)
  | "lax" -> (1.0, 1.0, 1.1, 1.0, 0.9, 1.0, 1.0, 1.0)
  | "relaxed" -> (1.0, 1.0, 1.1, 1.0, 1.0, 0.9, 1.0, 1.0)
  | "modest" -> (1.0, 0.9, 1.0, 1.1, 1.0, 1.0, 1.0, 1.0)
  | "mild" -> (1.0, 1.0, 0.9, 1.1, 1.0, 1.0, 1.0, 1.0)
  | "rash" -> (1.0, 1.0, 1.0, 1.1, 0.9, 1.0, 1.0, 1.0)
  | "quiet" -> (1.0, 1.0, 1.0, 1.1, 1.0, 0.9, 1.0, 1.0)
  | "calm" -> (1.0, 0.9, 1.0, 1.0, 1.1, 1.0, 1.0, 1.0)
  | "gentle" -> (1.0, 1.0, 0.9, 1.0, 1.1, 1.0, 1.0, 1.0)
  | "sassy" -> (1.0, 1.0, 1.0, 1.0, 1.1, 0.9, 1.0, 1.0)
  | "careful" -> (1.0, 1.0, 1.0, 0.9, 1.1, 1.0, 1.0, 1.0)
  | "timid" -> (1.0, 0.9, 1.0, 1.0, 1.0, 1.1, 1.0, 1.0)
  | "hasty" -> (1.0, 1.0, 0.9, 1.0, 1.0, 1.1, 1.0, 1.0)
  | "jolly" -> (1.0, 1.0, 1.0, 0.9, 1.0, 1.1, 1.0, 1.0)
  | "naive" -> (1.0, 1.0, 1.0, 1.0, 0.9, 1.1, 1.0, 1.0)
  | "hardy" -> (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "docile" -> (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "serious" -> (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "bashful" -> (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "quirky" -> (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | _ -> failwith "Unknown nature"

let get_multipliers_by_ailment = function
  | "burned" -> (1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | "paralyzed" -> (1.0, 1.0, 1.0, 1.0, 1.0, 0.25, 1.0, 1.0)
  | _ -> (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)

let get_multipliers_by_stat_stages stat_stages =
  let multiplier_by_stat_stage = function
    | -6 -> 0.25
    | -5 -> 2. /. 7.
    | -4 -> 2. /. 6.
    | -3 -> 0.4
    | -2 -> 0.5
    | -1 -> 2. /. 3.
    | 0 -> 1.
    | 1 -> 1.5
    | 2 -> 2.
    | 3 -> 2.5
    | 4 -> 3.
    | 5 -> 3.5
    | 6 -> 4.
    | _ -> failwith "Stat Multiplier out of range"
  in
  ( multiplier_by_stat_stage stat_stages.hp,
    multiplier_by_stat_stage stat_stages.atk,
    multiplier_by_stat_stage stat_stages.def,
    multiplier_by_stat_stage stat_stages.spatk,
    multiplier_by_stat_stage stat_stages.spdef,
    multiplier_by_stat_stage stat_stages.spd,
    multiplier_by_stat_stage stat_stages.acc,
    multiplier_by_stat_stage stat_stages.eva )

let apply_multiplier stat multiplier =
  int_of_float (float_of_int stat *. multiplier)

let apply_multipliers cur_stats
    ( hp_mult,
      atk_mult,
      def_mult,
      spatk_mult,
      spdef_mult,
      spd_mult,
      acc_mult,
      eva_mult ) =
  {
    hp = apply_multiplier cur_stats.hp hp_mult;
    atk = apply_multiplier cur_stats.atk atk_mult;
    def = apply_multiplier cur_stats.def def_mult;
    spatk = apply_multiplier cur_stats.spatk spatk_mult;
    spdef = apply_multiplier cur_stats.spdef spdef_mult;
    spd = apply_multiplier cur_stats.spd spd_mult;
    acc = apply_multiplier cur_stats.spd acc_mult;
    eva = apply_multiplier cur_stats.spd eva_mult;
  }

let rec apply_multipliers_list cur_stats multipliers =
  match multipliers with
  | [] -> cur_stats
  | h :: t -> apply_multipliers_list (apply_multipliers cur_stats h) t

let calc_current_stats base_stats nature level ailment stat_stages =
  let stat_aux base_stat level = ((2 * base_stat) + 31 + 21) * level / 100 in
  apply_multipliers_list
    {
      hp = stat_aux base_stats.hp level + level + 10;
      atk = stat_aux base_stats.atk level + 5;
      def = stat_aux base_stats.def level + 5;
      spatk = stat_aux base_stats.spatk level + 5;
      spdef = stat_aux base_stats.spdef level + 5;
      spd = stat_aux base_stats.spd level + 5;
      acc = 100;
      eva = 100;
    }
    [ get_multipliers_by_nature nature; get_multipliers_by_ailment ailment ]

let stats_to_list { hp; atk; def; spatk; spdef; spd; acc; eva } =
  [ hp; atk; def; spatk; spdef; spd; acc; eva ]

let swords_dance =
  {
    id = 14;
    name = "Swords Dance";
    tipe = Normal;
    power = 0;
    pp = 20;
    accuracy = 0;
    priority = 0;
    target = Self;
    damage_class = Status;
    effect_id = 51;
    effect_chance = 0;
  }

let razor_leaf =
  {
    id = 75;
    name = "Razor Leaf";
    tipe = Grass;
    power = 55;
    pp = 25;
    accuracy = 95;
    priority = 0;
    target = Enemy;
    damage_class = Physical;
    effect_id = 44;
    effect_chance = 0;
  }

let tackle =
  {
    id = 33;
    name = "Tackle";
    tipe = Normal;
    power = 40;
    pp = 35;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Physical;
    effect_id = 1;
    effect_chance = 0;
  }

let vine_whip =
  {
    id = 22;
    name = "Vine Whip";
    tipe = Grass;
    power = 45;
    pp = 25;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Physical;
    effect_id = 1;
    effect_chance = 0;
  }

let poison_powder =
  {
    id = 77;
    name = "Poison Powder";
    tipe = Poison;
    power = 0;
    pp = 35;
    accuracy = 75;
    priority = 0;
    target = Enemy;
    damage_class = Status;
    effect_id = 67;
    effect_chance = 0;
  }

let sleep_powder =
  {
    id = 79;
    name = "Sleep Powder";
    tipe = Grass;
    power = 0;
    pp = 15;
    accuracy = 75;
    priority = 0;
    target = Enemy;
    damage_class = Status;
    effect_id = 2;
    effect_chance = 0;
  }

let slash =
  {
    id = 163;
    name = "Slash";
    tipe = Normal;
    power = 70;
    pp = 20;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Physical;
    effect_id = 44;
    effect_chance = 0;
  }

let flamethrower =
  {
    id = 53;
    name = "Flamethrower";
    tipe = Fire;
    power = 90;
    pp = 15;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Special;
    effect_id = 5;
    effect_chance = 10;
  }

let ember =
  {
    id = 52;
    name = "Ember";
    tipe = Fire;
    power = 40;
    pp = 25;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Special;
    effect_id = 5;
    effect_chance = 10;
  }

let scratch =
  {
    id = 10;
    name = "Scratch";
    tipe = Normal;
    power = 40;
    pp = 35;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Physical;
    effect_id = 1;
    effect_chance = 0;
  }

let growl =
  {
    id = 45;
    name = "Growl";
    tipe = Normal;
    power = 0;
    pp = 40;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Status;
    effect_id = 19;
    effect_chance = 0;
  }

let smokescreen =
  {
    id = 108;
    name = "Smokescreen";
    tipe = Normal;
    power = 0;
    pp = 20;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Status;
    effect_id = 24;
    effect_chance = 0;
  }

let water_gun =
  {
    id = 55;
    name = "Water Gun";
    tipe = Water;
    power = 40;
    pp = 25;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Special;
    effect_id = 1;
    effect_chance = 0;
  }

let tail_whip =
  {
    id = 39;
    name = "Tail Whip";
    tipe = Normal;
    power = 0;
    pp = 30;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Status;
    effect_id = 20;
    effect_chance = 0;
  }

let ice_beam =
  {
    id = 58;
    name = "Ice Beam";
    tipe = Ice;
    power = 90;
    pp = 10;
    accuracy = 100;
    priority = 0;
    target = Enemy;
    damage_class = Special;
    effect_id = 6;
    effect_chance = 10;
  }

let move_identifiers =
  [
    (14, swords_dance);
    (75, razor_leaf);
    (33, tackle);
    (22, vine_whip);
    (77, poison_powder);
    (79, sleep_powder);
    (163, slash);
    (53, flamethrower);
    (52, ember);
    (10, scratch);
    (45, growl);
    (108, smokescreen);
    (55, water_gun);
    (39, tail_whip);
    (58, ice_beam);
  ]

let move_ids lst =
  let rec get_move_ids lst = function
    | [] -> []
    | h :: t ->
        if List.mem (snd h) lst then fst h :: get_move_ids lst t
        else get_move_ids lst t
  in
  get_move_ids lst move_identifiers

type p_info = {
  tipe : tipe * tipe;
  stats : stats;
  moves : move list;
  possible_abilities : string list;
}

let get_info_from_species species =
  match species with
  | "bulbasaur" ->
      {
        tipe = (Grass, Poison);
        stats =
          {
            hp = 45;
            atk = 49;
            def = 49;
            spatk = 65;
            spdef = 65;
            spd = 45;
            acc = 0;
            eva = 0;
          };
        moves =
          [
            swords_dance;
            razor_leaf;
            tackle;
            vine_whip;
            poison_powder;
            sleep_powder;
          ];
        possible_abilities = [ "overgrow"; "chlorophyll" ];
      }
  | "ivysaur" ->
      {
        tipe = (Grass, Poison);
        stats =
          {
            hp = 60;
            atk = 62;
            def = 63;
            spatk = 80;
            spdef = 80;
            spd = 60;
            acc = 0;
            eva = 0;
          };
        moves =
          [
            swords_dance;
            razor_leaf;
            tackle;
            vine_whip;
            poison_powder;
            sleep_powder;
          ];
        possible_abilities = [ "overgrow"; "chlorophyll" ];
      }
  | "venusaur" ->
      {
        tipe = (Grass, Poison);
        stats =
          {
            hp = 80;
            atk = 82;
            def = 83;
            spatk = 100;
            spdef = 100;
            spd = 80;
            acc = 0;
            eva = 0;
          };
        moves =
          [
            swords_dance;
            razor_leaf;
            tackle;
            vine_whip;
            poison_powder;
            sleep_powder;
          ];
        possible_abilities = [ "overgrow"; "chlorophyll" ];
      }
  | "charmander" ->
      {
        tipe = (Fire, NoneType);
        stats =
          {
            hp = 39;
            atk = 52;
            def = 43;
            spatk = 60;
            spdef = 50;
            spd = 65;
            acc = 0;
            eva = 0;
          };
        moves = [ slash; flamethrower; ember; scratch; growl; smokescreen ];
        possible_abilities = [ "blaze"; "solar-power" ];
      }
  | "charmeleon" ->
      {
        tipe = (Fire, NoneType);
        stats =
          {
            hp = 58;
            atk = 64;
            def = 58;
            spatk = 80;
            spdef = 65;
            spd = 80;
            acc = 0;
            eva = 0;
          };
        moves = [ slash; flamethrower; ember; scratch; growl; smokescreen ];
        possible_abilities = [ "blaze"; "solar-power" ];
      }
  | "charizard" ->
      {
        tipe = (Fire, Flying);
        stats =
          {
            hp = 78;
            atk = 84;
            def = 78;
            spatk = 109;
            spdef = 85;
            spd = 100;
            acc = 0;
            eva = 0;
          };
        moves = [ slash; flamethrower; ember; scratch; growl; smokescreen ];
        possible_abilities = [ "blaze"; "solar-power" ];
      }
  | "squirtle" ->
      {
        tipe = (Water, NoneType);
        stats =
          {
            hp = 44;
            atk = 48;
            def = 65;
            spatk = 50;
            spdef = 64;
            spd = 43;
            acc = 0;
            eva = 0;
          };
        moves = [ water_gun; tackle; tail_whip; ice_beam ];
        possible_abilities = [ "torrent"; "rain-dish" ];
      }
  | "wartortle" ->
      {
        tipe = (Water, NoneType);
        stats =
          {
            hp = 59;
            atk = 63;
            def = 80;
            spatk = 65;
            spdef = 80;
            spd = 58;
            acc = 0;
            eva = 0;
          };
        moves = [ water_gun; tackle; tail_whip; ice_beam ];
        possible_abilities = [ "torrent"; "rain-dish" ];
      }
  | "blastoise" ->
      {
        tipe = (Water, NoneType);
        stats =
          {
            hp = 79;
            atk = 83;
            def = 100;
            spatk = 85;
            spdef = 105;
            spd = 78;
            acc = 0;
            eva = 0;
          };
        moves = [ water_gun; tackle; tail_whip; ice_beam ];
        possible_abilities = [ "torrent"; "rain-dish" ];
      }
  | _ -> raise BadPokemon

let get_moves str = (get_info_from_species (String.lowercase_ascii str)).moves

let create name lvl nat =
  let info = get_info_from_species (String.lowercase_ascii name) in
  (*Raises BadPokemon if not a valid species*)
  if lvl < 1 || lvl > 100 || not (List.mem nat valid_natures) then
    raise BadPokemon
  else
    let cur_stats =
      calc_current_stats info.stats nat lvl "healthy" zero_stats
    in
    let is_dual_type =
      match info.tipe with
      | _, NoneType -> false
      | _ -> true
    in
    {
      species = String.lowercase_ascii name;
      is_dual_type;
      tipe = info.tipe;
      base_stats = info.stats;
      cur_stats;
      stat_stages = zero_stats;
      moves = [];
      level = lvl;
      ailment = "healthy";
      nature = String.lowercase_ascii nat;
      cur_hp = cur_stats.hp;
    }

let attack a d move =
  let attacker = create a.species a.level a.nature in
  let defender = create d.species d.level d.nature in
  (attacker, defender)
(*Change to actually calculate new stats*)

let apply_stat_change p stat_name num_stages =
  let new_stages =
    match stat_name with
    | "hp" -> { p.stat_stages with hp = p.stat_stages.hp + num_stages }
    | "atk" -> { p.stat_stages with atk = p.stat_stages.atk + num_stages }
    | "spatk" -> { p.stat_stages with spatk = p.stat_stages.spatk + num_stages }
    | "def" -> { p.stat_stages with def = p.stat_stages.def + num_stages }
    | "spdef" -> { p.stat_stages with spdef = p.stat_stages.spdef + num_stages }
    | "spd" -> { p.stat_stages with spd = p.stat_stages.spd + num_stages }
    | "acc" -> { p.stat_stages with acc = p.stat_stages.acc + num_stages }
    | "eva" -> { p.stat_stages with eva = p.stat_stages.eva + num_stages }
    | _ -> failwith "Unknown Stat Name"
  in
  {
    p with
    stat_stages = new_stages;
    cur_stats =
      calc_current_stats p.base_stats p.nature p.level p.ailment new_stages;
  }

let add_pokemon_move (pokemon : t) (new_move_id : int) : t =
  let new_move =
    let rec find_move id = function
      | [] -> raise (Failure "Unrecognized move id")
      | h :: t -> if fst h = id then snd h else find_move id t
    in
    find_move new_move_id move_identifiers
  in
  if List.length pokemon.moves >= 4 then
    raise (Failure "A Pokémon can only have up to 4 moves.")
  else if List.mem new_move (get_info_from_species pokemon.species).moves then
    { pokemon with moves = pokemon.moves @ [ new_move ] }
  else raise (Failure "This move is not permitted by this species")
