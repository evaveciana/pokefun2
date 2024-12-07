open Random

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
  mutable cur_hp : int;
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

(*returns list of move ids*)
let get_pokemon_id poke_name =
  let pokemon = Csv.load "lib/python/data/pokemon.csv" in

  (* Find the Pokémon ID based on the given name *)
  match List.find_opt (fun row -> List.nth row 1 = poke_name) pokemon with
  | Some row ->
      int_of_string (List.hd row) (* Assuming the ID is in the first column *)
  | None -> failwith "Not valid pokemon"

let get_move_ids pokemon_id =
  let (pokemon_moves : string list list) =
    Csv.load "lib/python/data/pokemon_moves.csv"
  in

  List.fold_left
    (fun acc elt ->
      if List.hd elt = string_of_int pokemon_id then
        int_of_string (List.nth elt 2) :: acc
      else acc)
    [] pokemon_moves

let get_type_name type_id =
  let types = Csv.load "lib/python/data/types.csv" in

  match
    List.find_opt (fun row -> List.hd row = string_of_int type_id) types
  with
  | Some [ _; name; _; _ ] -> name
  | _ -> "Unknown"

let get_move_id_from_name (move_name : string) : int =
  let moves = Csv.load "lib/python/data/moves.csv" in

  match List.find_opt (fun row -> List.nth row 1 = move_name) moves with
  | Some [ id; _; _; _; _; _; _; _; _; _; _; _; _; _; _ ] -> int_of_string id
  | _ -> failwith "Invalid move"

let tipe_to_string (tipe : tipe) : string =
  match tipe with
  | Normal -> "Normal"
  | Fighting -> "Fighting"
  | Flying -> "Flying"
  | Poison -> "Poison"
  | Ground -> "Ground"
  | Rock -> "Rock"
  | Bug -> "Bug"
  | Ghost -> "Ghost"
  | Steel -> "Steel"
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Electric -> "Electric"
  | Psychic -> "Psychic"
  | Ice -> "Ice"
  | Dragon -> "Dragon"
  | Dark -> "Dark"
  | Fairy -> "Fairy"
  | NoneType -> "NoneType"

let damage_class_to_string (damage_class : damage_class) : string =
  match damage_class with
  | Physical -> "Physical"
  | Special -> "Special"
  | Status -> "Status"

let move_to_string (move : move) : string =
  move.name ^ " (Type: " ^ tipe_to_string move.tipe ^ ", Power: "
  ^ string_of_int move.power ^ ", PP: " ^ string_of_int move.pp ^ ", Accuracy: "
  ^ string_of_int move.accuracy
  ^ ", Damage Class: "
  ^ damage_class_to_string move.damage_class
  ^ ")"

(*TODO FULLY IMPLEMENT LATER !!! just minimal for now*)
let pokemon_to_string (pokemon : t) : string =
  let moves =
    if List.length pokemon.moves = 0 then "No moves available."
    else pokemon.moves |> List.map move_to_string |> String.concat "\n"
  in
  pokemon.species ^ ": " ^ moves

let create_move_from_name move_name =
  let moves = Csv.load "lib/python/data/moves.csv" in

  match List.find_opt (fun row -> List.nth row 1 = move_name) moves with
  | Some
      [
        id;
        identifier;
        _;
        type_id;
        power;
        pp;
        accuracy;
        priority;
        target_id;
        damage_class_id;
        effect_id;
        effect_chance;
        _;
        _;
        _;
      ] ->
      {
        id = int_of_string id;
        name = identifier;
        tipe =
          (match int_of_string type_id with
          | 1 -> Normal
          | 2 -> Fighting
          | 3 -> Flying
          | 4 -> Poison
          | 5 -> Ground
          | 6 -> Rock
          | 7 -> Bug
          | 8 -> Ghost
          | 9 -> Steel
          | 10 -> Fire
          | 11 -> Water
          | 12 -> Grass
          | 13 -> Electric
          | 14 -> Psychic
          | 15 -> Ice
          | 16 -> Dragon
          | 17 -> Dark
          | 18 -> Fairy
          | _ -> failwith "Unknown type");
        power = int_of_string power;
        pp = int_of_string pp;
        accuracy = int_of_string accuracy;
        priority = int_of_string priority;
        target =
          (if int_of_string target_id = 10 then Enemy
           else if int_of_string target_id = 11 then Self
           else failwith "Not 10 or 11");
        damage_class =
          (if int_of_string damage_class_id = 1 then Status
           else if int_of_string damage_class_id = 2 then Physical
           else if int_of_string damage_class_id = 3 then Special
           else failwith "Not 1, 2, or 3");
        effect_id = int_of_string effect_id;
        effect_chance = int_of_string effect_chance;
      }
  | Some _ -> failwith "TODO"
  | None -> failwith "Not valid move_id"

let create_move_from_id move_id =
  let moves = Csv.load "lib/python/data/moves.csv" in

  match List.find_opt (fun row -> List.hd row = move_id) moves with
  | Some
      [
        id;
        identifier;
        _;
        type_id;
        power;
        pp;
        accuracy;
        priority;
        target_id;
        damage_class_id;
        effect_id;
        effect_chance;
        _;
        _;
        _;
      ] ->
      {
        id = int_of_string id;
        name = identifier;
        tipe =
          (match int_of_string type_id with
          | 1 -> Normal
          | 2 -> Fighting
          | 3 -> Flying
          | 4 -> Poison
          | 5 -> Ground
          | 6 -> Rock
          | 7 -> Bug
          | 8 -> Ghost
          | 9 -> Steel
          | 10 -> Fire
          | 11 -> Water
          | 12 -> Grass
          | 13 -> Electric
          | 14 -> Psychic
          | 15 -> Ice
          | 16 -> Dragon
          | 17 -> Dark
          | 18 -> Fairy
          | _ -> failwith "Unknown type");
        power = int_of_string power;
        pp = int_of_string pp;
        accuracy = int_of_string accuracy;
        priority = int_of_string priority;
        target =
          (if int_of_string target_id = 10 then Enemy
           else if int_of_string target_id = 11 then Self
           else failwith "Not 10 or 11");
        damage_class =
          (if int_of_string damage_class_id = 1 then Status
           else if int_of_string damage_class_id = 2 then Physical
           else if int_of_string damage_class_id = 3 then Special
           else failwith "Not 1, 2, or 3");
        effect_id = int_of_string effect_id;
        effect_chance = int_of_string effect_chance;
      }
  | Some _ -> failwith "TODO"
  | None -> failwith "Not valid move_id"

let display_learnable_moves (pokemon_species : string) : unit =
  let poke_id = get_pokemon_id pokemon_species in
  let move_ids = get_move_ids poke_id in
  let moves = Csv.load "lib/python/data/moves.csv" in

  let rec find_details move_id =
    match
      List.find_opt (fun row -> List.hd row = string_of_int move_id) moves
    with
    | Some
        [
          id;
          identifier;
          _;
          type_id;
          power;
          pp;
          accuracy;
          priority;
          target_id;
          damage_class_id;
          effect_id;
          effect_chance;
          _;
          _;
          _;
        ] ->
        let move_type = get_type_name (int_of_string type_id) in
        let damage_class =
          match int_of_string damage_class_id with
          | 1 -> "Status"
          | 2 -> "Physical"
          | 3 -> "Special"
          | _ -> "Unknown"
        in
        Printf.sprintf
          "%s (Type: %s, Power: %s, PP: %s, Accuracy: %s, Damage Class: %s)"
          identifier move_type power pp accuracy damage_class
    | _ -> "Unknown move"
  in
  print_endline ("Learnable moves for " ^ pokemon_species ^ ":");
  List.iter
    (fun move_id ->
      let move_details = find_details move_id in
      print_endline ("- " ^ move_details))
    move_ids

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

let move_identifiers = [ (22, vine_whip); (77, poison_powder) ]
let example_move () = vine_whip

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

let get_info_from_species (species : string) : p_info = failwith "TODO"
let get_moves str = (get_info_from_species (String.lowercase_ascii str)).moves

let create name lvl nat =
  let pokemon_tipes = Csv.load "lib/python/data/pokemon_types.csv" in
  let pokemon_id = get_pokemon_id name in

  let filter_by_int data id =
    List.filter
      (fun row ->
        match row with
        | [] -> false
        | hd :: _ -> ( try int_of_string hd = id with Failure _ -> false))
      data
  in

  let tipe =
    let tipes = filter_by_int pokemon_tipes pokemon_id in
    let match_tipe tipe_id =
      match tipe_id with
      | 1 -> Normal
      | 2 -> Fighting
      | 3 -> Flying
      | 4 -> Poison
      | 5 -> Ground
      | 6 -> Rock
      | 7 -> Bug
      | 8 -> Ghost
      | 9 -> Steel
      | 10 -> Fire
      | 11 -> Water
      | 12 -> Grass
      | 13 -> Electric
      | 14 -> Psychic
      | 15 -> Ice
      | 16 -> Dragon
      | 17 -> Dark
      | 18 -> Fairy
      | _ -> failwith "Unknown type"
    in

    let first_tipe = match_tipe (int_of_string (List.nth (List.hd tipes) 1)) in
    if List.length tipes = 1 then (first_tipe, NoneType)
    else
      let second_tipe =
        match_tipe (int_of_string (List.nth (List.nth tipes 2) 1))
      in
      (first_tipe, second_tipe)
  in

  let pokemon_stats = Csv.load "lib/python/data/pokemon_stats.csv" in

  let stat_list = filter_by_int pokemon_stats pokemon_id in
  let base_stats =
    {
      hp = int_of_string (List.nth (List.hd stat_list) 2);
      atk = int_of_string (List.nth (List.nth stat_list 1) 2);
      def = int_of_string (List.nth (List.nth stat_list 2) 2);
      spatk = int_of_string (List.nth (List.nth stat_list 3) 2);
      spdef = int_of_string (List.nth (List.nth stat_list 4) 2);
      spd = int_of_string (List.nth (List.nth stat_list 5) 2);
      acc = 100;
      eva = 100;
    }
  in
  let cur_stats = calc_current_stats base_stats nat lvl "healthy" zero_stats in
  (*Raises BadPokemon if not a valid species*)
  if lvl < 1 || lvl > 100 || not (List.mem nat valid_natures) then
    raise BadPokemon
  else
    let is_dual_type =
      match tipe with
      | _, NoneType -> false
      | _ -> true
    in
    {
      species = String.lowercase_ascii name;
      is_dual_type;
      tipe;
      base_stats;
      cur_stats;
      stat_stages = zero_stats;
      moves = [];
      level = lvl;
      ailment = "healthy";
      nature = String.lowercase_ascii nat;
      cur_hp = cur_stats.hp;
    }

let calc_effectiveness_mult (move : move) (defender : t) =
  let multiplier move_type target_type =
    match (move_type, target_type) with
    (* Normal *)
    | Normal, Rock | Normal, Steel -> 0.5
    | Normal, Ghost -> 0.0
    | Normal, _ -> 1.0
    (* Fire *)
    | Fire, Fire | Fire, Water | Fire, Rock | Fire, Dragon -> 0.5
    | Fire, Grass | Fire, Bug | Fire, Ice | Fire, Steel -> 2.0
    | Fire, _ -> 1.0
    (* Water *)
    | Water, Water | Water, Grass | Water, Dragon -> 0.5
    | Water, Fire | Water, Ground | Water, Rock -> 2.0
    | Water, _ -> 1.0
    (* Electric *)
    | Electric, Electric | Electric, Grass | Electric, Dragon -> 0.5
    | Electric, Water | Electric, Flying -> 2.0
    | Electric, Ground -> 0.0
    | Electric, _ -> 1.0
    (* Grass *)
    | Grass, Fire
    | Grass, Grass
    | Grass, Poison
    | Grass, Flying
    | Grass, Bug
    | Grass, Dragon
    | Grass, Steel -> 0.5
    | Grass, Water | Grass, Ground | Grass, Rock -> 2.0
    | Grass, _ -> 1.0
    (* Ice *)
    | Ice, Fire | Ice, Water | Ice, Ice | Ice, Steel -> 0.5
    | Ice, Grass | Ice, Ground | Ice, Flying | Ice, Dragon -> 2.0
    | Ice, _ -> 1.0
    (* Fighting *)
    | Fighting, Poison
    | Fighting, Flying
    | Fighting, Psychic
    | Fighting, Bug
    | Fighting, Fairy -> 0.5
    | Fighting, Normal
    | Fighting, Rock
    | Fighting, Steel
    | Fighting, Ice
    | Fighting, Dark -> 2.0
    | Fighting, Ghost -> 0.0
    | Fighting, _ -> 1.0
    (* Poison *)
    | Poison, Poison | Poison, Ground | Poison, Rock | Poison, Ghost -> 0.5
    | Poison, Grass | Poison, Fairy -> 2.0
    | Poison, Steel -> 0.0
    | Poison, _ -> 1.0
    (* Ground *)
    | Ground, Grass | Ground, Bug -> 0.5
    | Ground, Fire
    | Ground, Electric
    | Ground, Poison
    | Ground, Rock
    | Ground, Steel -> 2.0
    | Ground, Flying -> 0.0
    | Ground, _ -> 1.0
    (* Flying *)
    | Flying, Electric | Flying, Rock | Flying, Steel -> 0.5
    | Flying, Grass | Flying, Fighting | Flying, Bug -> 2.0
    | Flying, _ -> 1.0
    (* Psychic *)
    | Psychic, Psychic | Psychic, Steel -> 0.5
    | Psychic, Fighting | Psychic, Poison -> 2.0
    | Psychic, Dark -> 0.0
    | Psychic, _ -> 1.0
    (* Bug *)
    | Bug, Fire
    | Bug, Fighting
    | Bug, Poison
    | Bug, Flying
    | Bug, Ghost
    | Bug, Steel
    | Bug, Fairy -> 0.5
    | Bug, Grass | Bug, Psychic | Bug, Dark -> 2.0
    | Bug, _ -> 1.0
    (* Rock *)
    | Rock, Fighting | Rock, Ground | Rock, Steel -> 0.5
    | Rock, Fire | Rock, Ice | Rock, Flying | Rock, Bug -> 2.0
    | Rock, _ -> 1.0
    (* Ghost *)
    | Ghost, Dark -> 0.5
    | Ghost, Ghost | Ghost, Psychic -> 2.0
    | Ghost, Normal -> 0.0
    | Ghost, _ -> 1.0
    (* Dragon *)
    | Dragon, Steel -> 0.5
    | Dragon, Dragon -> 2.0
    | Dragon, Fairy -> 0.0
    | Dragon, _ -> 1.0
    (* Dark *)
    | Dark, Fighting | Dark, Dark | Dark, Fairy -> 0.5
    | Dark, Psychic | Dark, Ghost -> 2.0
    | Dark, _ -> 1.0
    (* Steel *)
    | Steel, Fire | Steel, Water | Steel, Electric | Steel, Steel -> 0.5
    | Steel, Ice | Steel, Rock | Steel, Fairy -> 2.0
    | Steel, _ -> 1.0
    (* Fairy *)
    | Fairy, Fire | Fairy, Poison | Fairy, Steel -> 0.5
    | Fairy, Fighting | Fairy, Dragon | Fairy, Dark -> 2.0
    | Fairy, _ -> 1.0
    | _, _ -> failwith "This should not happen. just for exhaustiveness"
  in

  let move_type = move.tipe in
  let type1, type2 = defender.tipe in

  let eff1 = multiplier move_type type1 in
  let eff2 =
    match type2 with
    | NoneType -> 1.0
    | _ -> multiplier move_type type2
  in

  eff1 *. eff2

let attack a d move =
  let attacker = create a.species a.level a.nature in
  let defender = create d.species d.level d.nature in
  let move_damage_class = move.damage_class in
  let match_dmg move_damage_class =
    match move_damage_class with
    | Physical ->
        let attack_stat = attacker.cur_stats.atk in
        let defense_stat = defender.cur_stats.def in
        (attack_stat, defense_stat)
    | Special ->
        let attack_stat = attacker.cur_stats.spatk in
        let defense_stat = defender.cur_stats.spdef in
        (attack_stat, defense_stat)
    | Status -> failwith "TODO"
  in
  let a_stat =
    match match_dmg move_damage_class with
    | attack, _ -> float_of_int attack
  in
  let d_stat =
    match match_dmg move_damage_class with
    | _, defense -> float_of_int defense
  in
  (*for now just use stage 1*)
  let calculate_crit = Random.int 16 in
  let crit = if calculate_crit = 0 then 1.5 else 1. in
  let rand = float_of_int (85 + Random.int (100 - 85 + 1)) /. 100.0 in
  (*"same type attack bonus" or STAB*)
  let stab =
    match attacker.tipe with
    | t1, t2 -> if t1 = move.tipe || t2 = move.tipe then 1.5 else 1.
  in
  let eff_mult = calc_effectiveness_mult move d in
  let dmg =
    (((2.0 *. float_of_int attacker.level /. 5.0) +. 2.0)
     *. float_of_int move.power *. (a_stat /. d_stat) /. 50.0
    +. 2.0)
    *. crit *. rand *. stab *. eff_mult
  in
  defender.cur_hp <- max 0 (int_of_float (float_of_int defender.cur_hp -. dmg));
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
