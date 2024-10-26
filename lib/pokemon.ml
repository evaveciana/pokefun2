type stats = {
  hp : int;
  atk : int;
  spatk : int;
  def : int;
  spdef : int;
  spd : int;
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
  nature : nature;
}

let zero_stats = { hp = 0; atk = 0; spatk = 0; def = 0; spdef = 0; spd = 0 }

let basic_move =
  {
    id = 0;
    name = "";
    tipe = Water;
    power = 0;
    pp = 0;
    accuracy = 0;
    priority = 0;
    target = Enemy;
    damage_class = Physical;
    effect_id = 0;
    effect_chance = 0;
  }

let basic_tipe = (Grass, Poison)
let basic_nature = Hardy

let create name level =
  {
    species = name;
    is_dual_type = false;
    tipe = (Water, NoneType);
    base_stats = zero_stats;
    cur_stats = zero_stats;
    stat_stages = zero_stats;
    moves = [];
    level;
    ailment = Healthy;
    nature = Jolly;
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
let get_stats_from_species species = zero_stats
let calc_stats base_stats nature level = zero_stats

type p_info = {
  tipe : tipe * tipe;
  stats : stats;
  moves : move list;
  possible_abilities : string list;
}

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

let get_info_from_species species =
  match species with
  | "bulbasaur" ->
      {
        tipe = (Grass, Poison);
        stats =
          { hp = 45; atk = 49; def = 49; spatk = 65; spdef = 65; spd = 45 };
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
          { hp = 60; atk = 62; def = 63; spatk = 80; spdef = 80; spd = 60 };
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
          { hp = 80; atk = 82; def = 83; spatk = 100; spdef = 100; spd = 80 };
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
          { hp = 39; atk = 52; def = 43; spatk = 60; spdef = 50; spd = 65 };
        moves = [ slash; flamethrower; ember; scratch; growl; smokescreen ];
        possible_abilities = [ "blaze"; "solar-power" ];
      }
  | "charmeleon" ->
      {
        tipe = (Fire, NoneType);
        stats =
          { hp = 58; atk = 64; def = 58; spatk = 80; spdef = 65; spd = 80 };
        moves = [ slash; flamethrower; ember; scratch; growl; smokescreen ];
        possible_abilities = [ "blaze"; "solar-power" ];
      }
  | "charizard" ->
      {
        tipe = (Fire, Flying);
        stats =
          { hp = 78; atk = 84; def = 78; spatk = 109; spdef = 85; spd = 100 };
        moves = [ slash; flamethrower; ember; scratch; growl; smokescreen ];
        possible_abilities = [ "blaze"; "solar-power" ];
      }
  | "squirtle" ->
      {
        tipe = (Water, NoneType);
        stats =
          { hp = 44; atk = 48; def = 65; spatk = 50; spdef = 64; spd = 43 };
        moves = [ water_gun; tackle; tail_whip; ice_beam ];
        possible_abilities = [ "torrent"; "rain-dish" ];
      }
  | "wartortle" ->
      {
        tipe = (Water, NoneType);
        stats =
          { hp = 59; atk = 63; def = 80; spatk = 65; spdef = 80; spd = 58 };
        moves = [ water_gun; tackle; tail_whip; ice_beam ];
        possible_abilities = [ "torrent"; "rain-dish" ];
      }
  | "blastoise" ->
      {
        tipe = (Water, NoneType);
        stats =
          { hp = 79; atk = 83; def = 100; spatk = 85; spdef = 105; spd = 78 };
        moves = [ water_gun; tackle; tail_whip; ice_beam ];
        possible_abilities = [ "torrent"; "rain-dish" ];
      }
  | _ -> failwith "Unknown Pokemon"
