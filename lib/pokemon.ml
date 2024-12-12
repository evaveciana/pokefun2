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

(* type tipe = | Normal | Fire | Water | Grass | Electric | Ice | Fighting |
   Poison | Ground | Flying | Psychic | Bug | Rock | Ghost | Dark | Dragon |
   Steel | Fairy | NoneType *)

(* type nature = | Hardy | Lonely | Brave | Adamant | Naughty | Bold | Docile |
   Relaxed | Impish | Lax | Timid | Hasty | Serious | Jolly | Naive | Modest |
   Mild | Quiet | Bashful | Rash | Calm | Gentle | Sassy | Careful | Quirky *)

(* let valid_natures = [ "hardy"; "lonely"; "brave"; "adamant"; "naughty";
   "bold"; "docile"; "relaxed"; "impish"; "lax"; "timid"; "hasty"; "serious";
   "jolly"; "naive"; "modest"; "mild"; "quiet"; "bashful"; "rash"; "calm";
   "gentle"; "sassy"; "careful"; "quirky"; ] *)

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
  tipe : string;
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
  tipe : string * string;
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

(* let species p = p.species let base_stats p = p.base_stats*)
let cur_stats p = p.cur_stats

(* Modify with ailment and stat stages *)
(*let base_hp p = *)
(* p.base_stats.hp let base_atk p = p.base_stats.atk let base_spatk p =
   p.base_stats.spatk let base_def p = p.base_stats.def let base_spdef p =
   p.base_stats.spdef let base_spd p = p.base_stats.spd let hp p =
   p.cur_stats.hp let max_hp p = 0 (* TODO*) let atk p = p.cur_stats.atk let def
   p = p.cur_stats.def let spatk p = p.cur_stats.spatk let spdef p =
   p.cur_stats.spdef let spd p = p.cur_stats.spd*)
let moves p = p.moves

let stats_to_list stats =
  [
    stats.hp;
    stats.atk;
    stats.def;
    stats.spatk;
    stats.spdef;
    stats.spd;
    stats.acc;
    stats.eva;
  ]

let list_to_stats = function
  | [ hp; atk; def; spatk; spdef; spd; acc; eva ] ->
      { hp; atk; def; spatk; spdef; spd; acc; eva }
  | _ -> failwith "Invalid stats list"

let search_csv_helper_one_match filename match_col key target_col =
  let file = Csv.load filename in
  match List.find_opt (fun row -> List.nth row match_col = key) file with
  | Some row -> List.nth row target_col
  | None -> failwith "not found"

let search_csv_helper_many_matches filename match_col key target_col =
  let file = Csv.load filename in
  List.fold_left
    (fun acc elt ->
      if List.nth elt match_col = key then List.nth elt target_col :: acc
      else acc)
    [] file

let search_csv_helper_whole_row filename match_col key start_col =
  let file = Csv.load filename in
  match List.find_opt (fun row -> List.nth row match_col = key) file with
  | Some row -> List.filteri (fun i _ -> i >= start_col) row
  | None -> failwith "none found"

let get_multipliers_by_nature n =
  let multipliers =
    search_csv_helper_whole_row "../data/multipliers_by_nature" 0 n 1
  in
  List.map float_of_string multipliers

let get_multipliers_by_ailment a =
  try
    let multipliers =
      search_csv_helper_whole_row "../data/multipliers_by_ailment" 0 a 1
    in
    List.map float_of_string multipliers
  with Failure _ -> [ 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0 ]

let get_multipliers_by_stat_stages stat_stages =
  let multiplier_by_stat_stage s =
    let multiplier =
      search_csv_helper_one_match "../data/multipliers_by_stat_stages" 0
        (string_of_int s) 1
    in
    float_of_string multiplier
  in
  List.map multiplier_by_stat_stage (stats_to_list stat_stages)

let apply_multiplier stat multiplier =
  int_of_float (float_of_int stat *. multiplier)

let apply_multipliers cur_stats mult_list =
  let applied =
    List.map2 apply_multiplier (stats_to_list cur_stats) mult_list
  in
  list_to_stats applied

let rec apply_multipliers_list cur_stats multipliers =
  match multipliers with
  | [] -> cur_stats
  | h :: t -> apply_multipliers_list (apply_multipliers cur_stats h) t

let calc_current_stats base_stats nature level ailment (stat_stages : stats) =
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

let get_pokemon_id poke_name =
  let id = search_csv_helper_one_match "../data/pokemon.csv" 1 poke_name 0 in
  int_of_string id

let get_move_ids pokemon_id =
  let match_list =
    search_csv_helper_many_matches "../data/pokemon_moves.csv" 0
      (string_of_int pokemon_id) 2
  in
  List.map int_of_string match_list

let get_type_name type_id =
  search_csv_helper_one_match "../data/types.csv" 0 (string_of_int type_id) 1

let get_move_id_from_name move_name =
  let id = search_csv_helper_one_match "../data/moves.csv" 1 move_name 0 in
  int_of_string id

(* let tipe_to_string = function | Normal -> "Normal" | Fighting -> "Fighting" |
   Flying -> "Flying" | Poison -> "Poison" | Ground -> "Ground" | Rock -> "Rock"
   | Bug -> "Bug" | Ghost -> "Ghost" | Steel -> "Steel" | Fire -> "Fire" | Water
   -> "Water" | Grass -> "Grass" | Electric -> "Electric" | Psychic -> "Psychic"
   | Ice -> "Ice" | Dragon -> "Dragon" | Dark -> "Dark" | Fairy -> "Fairy" |
   NoneType -> "NoneType"

   let string_to_tipe = function | "Normal" -> Normal | "Fighting" -> Fighting |
   "Flying" -> Flying | "Poison" -> Poison | "Ground" -> Ground | "Rock" -> Rock
   | "Bug" -> Bug | "Ghost" -> Ghost | "Steel" -> Steel | "Fire" -> Fire |
   "Water" -> Water | "Grass" -> Grass | "Electric" -> Electric | "Psychic" ->
   Psychic | "Ice" -> Ice | "Dragon" -> Dragon | "Dark" -> Dark | "Fairy" ->
   Fairy | "NoneType" -> NoneType | _ -> failwith "Invalid tipe" *)

let damage_class_to_string (damage_class : damage_class) : string =
  match damage_class with
  | Physical -> "Physical"
  | Special -> "Special"
  | Status -> "Status"

let move_to_string move =
  move.name ^ " (Type: " ^ move.tipe ^ ", Power: " ^ string_of_int move.power
  ^ ", PP: " ^ string_of_int move.pp ^ ", Accuracy: "
  ^ string_of_int move.accuracy
  ^ ", Damage Class: "
  ^ damage_class_to_string move.damage_class
  ^ ")"

(*TODO FULLY IMPLEMENT LATER !!! just minimal for now*)
let pokemon_to_string pokemon =
  let moves =
    if List.length pokemon.moves = 0 then "No moves available."
    else pokemon.moves |> List.map move_to_string |> String.concat "\n"
  in
  pokemon.species ^ ": " ^ moves

let create_move_from_name move_name =
  match search_csv_helper_whole_row "../data/moves.csv" 1 move_name 0 with
  | [
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
        tipe = get_type_name (int_of_string type_id);
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
  | _ -> failwith "Invalid move ID"

let display_learnable_moves pokemon_species =
  let poke_id = get_pokemon_id pokemon_species in
  let move_ids = get_move_ids poke_id in
  let moves = Csv.load "../data/moves.csv" in

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
    tipe = "Grass";
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
    tipe = "Poison";
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
  tipe : string * string;
  stats : stats;
  moves : move list;
  possible_abilities : string list;
}

let get_info_from_species (species : string) : p_info = failwith "TODO"
let get_moves str = (get_info_from_species (String.lowercase_ascii str)).moves

let create name lvl nat =
  let pokemon_tipes = Csv.load "../data/pokemon_types.csv" in
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
      search_csv_helper_one_match "data/types.csv" 0 (string_of_int tipe_id) 1
    in

    let first_tipe = match_tipe (int_of_string (List.nth (List.hd tipes) 1)) in
    if List.length tipes = 1 then (first_tipe, "NoneType")
    else
      let second_tipe =
        match_tipe (int_of_string (List.nth (List.nth tipes 2) 1))
      in
      (first_tipe, second_tipe)
  in

  let pokemon_stats = Csv.load "../data/pokemon_stats.csv" in

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
  try
    ignore (search_csv_helper_one_match "data/multipliers_by_nature" 0 nat 0);
    if lvl < 1 || lvl > 100 then raise BadPokemon
    else
      let is_dual_type = if snd tipe = "NoneType" then false else true in

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
  with Failure _ -> raise BadPokemon

let calc_effectiveness_mult (move : move) (defender : t) =
  let multiplier move_type target_type =
    if move_type = "Normal" then
      if target_type = "Rock" || target_type = "Steel" then 0.5
      else if target_type = "Ghost" then 0.0
      else 1.0
    else if move_type = "Fire" then
      if
        target_type = "Fire" || target_type = "Water" || target_type = "Rock"
        || target_type = "Dragon"
      then 0.5
      else if
        target_type = "Grass" || target_type = "Bug" || target_type = "Ice"
        || target_type = "Steel"
      then 2.0
      else 1.0
    else if move_type = "Water" then
      if
        target_type = "Water" || target_type = "Grass" || target_type = "Dragon"
      then 0.5
      else if
        target_type = "Fire" || target_type = "Ground" || target_type = "Rock"
      then 2.0
      else 1.0
    else if move_type = "Electric" then
      if
        target_type = "Electric" || target_type = "Grass"
        || target_type = "Dragon"
      then 0.5
      else if target_type = "Water" || target_type = "Flying" then 2.0
      else if target_type = "Ground" then 0.0
      else 1.0
    else if move_type = "Grass" then
      if
        target_type = "Fire" || target_type = "Grass" || target_type = "Poison"
        || target_type = "Flying" || target_type = "Bug"
        || target_type = "Dragon" || target_type = "Steel"
      then 0.5
      else if
        target_type = "Water" || target_type = "Ground" || target_type = "Rock"
      then 2.0
      else 1.0
    else if move_type = "Ice" then
      if
        target_type = "Fire" || target_type = "Water" || target_type = "Ice"
        || target_type = "Steel"
      then 0.5
      else if
        target_type = "Grass" || target_type = "Ground"
        || target_type = "Flying" || target_type = "Dragon"
      then 2.0
      else 1.0
    else if move_type = "Fighting" then
      if
        target_type = "Poison" || target_type = "Flying"
        || target_type = "Psychic" || target_type = "Bug"
        || target_type = "Fairy"
      then 0.5
      else if
        target_type = "Normal" || target_type = "Rock" || target_type = "Steel"
        || target_type = "Ice" || target_type = "Dark"
      then 2.0
      else if target_type = "Ghost" then 0.0
      else 1.0
    else if move_type = "Poison" then
      if
        target_type = "Poison" || target_type = "Ground" || target_type = "Rock"
        || target_type = "Ghost"
      then 0.5
      else if target_type = "Grass" || target_type = "Fairy" then 2.0
      else if target_type = "Steel" then 0.0
      else 1.0
    else if move_type = "Ground" then
      if target_type = "Grass" || target_type = "Bug" then 0.5
      else if
        target_type = "Fire" || target_type = "Electric"
        || target_type = "Poison" || target_type = "Rock"
        || target_type = "Steel"
      then 2.0
      else if target_type = "Flying" then 0.0
      else 1.0
    else if move_type = "Flying" then
      if
        target_type = "Electric" || target_type = "Rock"
        || target_type = "Steel"
      then 0.5
      else if
        target_type = "Grass" || target_type = "Fighting" || target_type = "Bug"
      then 2.0
      else 1.0
    else if move_type = "Psychic" then
      if target_type = "Psychic" || target_type = "Steel" then 0.5
      else if target_type = "Fighting" || target_type = "Poison" then 2.0
      else if target_type = "Dark" then 0.0
      else 1.0
    else if move_type = "Bug" then
      if
        target_type = "Fire" || target_type = "Fighting"
        || target_type = "Poison" || target_type = "Flying"
        || target_type = "Ghost" || target_type = "Steel"
        || target_type = "Fairy"
      then 0.5
      else if
        target_type = "Grass" || target_type = "Psychic" || target_type = "Dark"
      then 2.0
      else 1.0
    else if move_type = "Rock" then
      if
        target_type = "Fighting" || target_type = "Ground"
        || target_type = "Steel"
      then 0.5
      else if
        target_type = "Fire" || target_type = "Ice" || target_type = "Flying"
        || target_type = "Bug"
      then 2.0
      else 1.0
    else if move_type = "Ghost" then
      if target_type = "Dark" then 0.5
      else if target_type = "Ghost" || target_type = "Psychic" then 2.0
      else if target_type = "Normal" then 0.0
      else 1.0
    else if move_type = "Dragon" then
      if target_type = "Steel" then 0.5
      else if target_type = "Dragon" then 2.0
      else if target_type = "Dairy" then 0.0
      else 1.0
    else if move_type = "Dark" then
      if
        target_type = "Fighting" || target_type = "Dark"
        || target_type = "Fairy"
      then 0.5
      else if target_type = "Psychic" || target_type = "Ghost" then 2.0
      else 1.0
    else if move_type = "Steel" then
      if
        target_type = "Fire" || target_type = "Water"
        || target_type = "Electric" || target_type = "Steel"
      then 0.5
      else if
        target_type = "Ice" || target_type = "Rock" || target_type = "Fairy"
      then 2.0
      else 1.0
    else if move_type = "Fairy" then
      if target_type = "Fire" || target_type = "Poison" || target_type = "Steel"
      then 0.5
      else if
        target_type = "Ice" || target_type = "Rock" || target_type = "Fairy"
      then 2.0
      else 1.0
    else failwith "Invalid type"
  in

  let move_type = move.tipe in
  let type1, type2 = defender.tipe in

  let eff1 = multiplier move_type type1 in
  let eff2 = if type2 = "NoneType" then 1.0 else multiplier move_type type2 in

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
