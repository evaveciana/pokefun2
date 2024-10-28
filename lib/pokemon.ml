type stats = {
  hp : int;
  atk : int;
  spatk : int;
  def : int;
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
  nature : string;
}

exception BadPokemon

let zero_stats =
  { hp = 0; atk = 0; spatk = 0; def = 0; spdef = 0; spd = 0; acc = 0; eva = 0 }

(* let basic_move = { id = 0; name = ""; tipe = Water; power = 0; pp = 0;
   accuracy = 0; priority = 0; target = Enemy; damage_class = Physical;
   effect_id = 0; effect_chance = 0; } *)

(* let basic_tipe = (Grass, Poison) *)
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
let spatk p = p.cur_stats.spatk
let def p = p.cur_stats.def
let spdef p = p.cur_stats.spdef
let spd p = p.cur_stats.spd

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
  | Burned -> (1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  | Paralyzed -> (1.0, 1.0, 1.0, 1.0, 1.0, 0.25, 1.0, 1.0)
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
    spatk = apply_multiplier cur_stats.spatk spatk_mult;
    def = apply_multiplier cur_stats.def def_mult;
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
      atk = stat_aux base_stats.hp level + 5;
      def = stat_aux base_stats.hp level + 5;
      spatk = stat_aux base_stats.hp level + 5;
      spdef = stat_aux base_stats.hp level + 5;
      spd = stat_aux base_stats.hp level + 5;
      acc = 100;
      eva = 100;
    }
    [ get_multipliers_by_nature nature; get_multipliers_by_ailment ailment ]

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

let get_moves str = (get_info_from_species str).moves
(* Necessary? *)

let create name mvlst lvl nat =
  let info = get_info_from_species name in
  (*Raises BadPokemon if not a valid species*)
  let rec check_mvlst = function
    | [] -> true
    | h :: t -> List.mem h info.moves && check_mvlst t
  in
  if
    lvl < 1 || lvl > 100
    || (not (check_mvlst mvlst))
    || not (List.mem nat valid_natures)
  then raise BadPokemon
  else
    {
      species = name;
      is_dual_type = false;
      (*change*)
      tipe = info.tipe;
      base_stats = info.stats;
      cur_stats = calc_current_stats info.stats nat lvl Healthy zero_stats;
      stat_stages = zero_stats;
      moves = mvlst;
      level = lvl;
      ailment = Healthy;
      nature = String.lowercase_ascii nat;
    }

let attack a d move =
  let attacker = create a.species a.moves a.level a.nature in
  let defender = create d.species d.moves d.level d.nature in
  (attacker, defender)
(*Change to actually calculate new stats*)

let apply_status_effect p stat_name num_stages =
  let new_stats =
    match stat_name with
    (*change these to actually calculate new stats*)
    | "hp" -> { p.cur_stats with hp = 1 }
    | "atk" -> { p.cur_stats with atk = 1 }
    | "spatk" -> { p.cur_stats with spatk = 1 }
    | "def" -> { p.cur_stats with def = 1 }
    | "spdef" -> { p.cur_stats with spdef = 1 }
    | "spd" -> { p.cur_stats with spd = 1 }
    | "acc" -> { p.cur_stats with acc = 1 }
    | "eva" -> { p.cur_stats with eva = 1 }
    | _ -> failwith "todo"
  in
  { p with cur_stats = new_stats }

(* How do you use num_stages? *)

let add_pokemon_move (pokemon : t) (new_move : move) : t =
  if List.length pokemon.moves >= 4 then
    raise (Failure "A Pokémon can only have up to 4 moves.")
  else { pokemon with moves = pokemon.moves @ [ new_move ] }
