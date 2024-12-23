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

let one_stats =
  { hp = 1; atk = 1; def = 1; spatk = 1; spdef = 1; spd = 1; acc = 0; eva = 0 }

let cur_stats p = p.cur_stats
let atk p = p.cur_stats.atk
let def p = p.cur_stats.def
let spatk p = p.cur_stats.spatk
let spdef p = p.cur_stats.spdef
let spd p = p.cur_stats.spd
let max_hp p = p.cur_stats.hp
let moves p = p.moves
let spd p = p.cur_stats.spd
let cur_hp p = p.cur_hp

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
  try
    let file = Csv.load filename in
    match List.find_opt (fun row -> List.nth row match_col = key) file with
    | Some row -> List.nth row target_col
    | None -> failwith ("not found: " ^ key ^ " file: " ^ filename)
  with Failure _ ->
    raise (Failure "Attempting to match to column out of range one_match")

let search_csv_helper_many_matches filename match_col key target_col =
  try
    let file = Csv.load filename in
    List.fold_left
      (fun acc elt ->
        if List.nth elt match_col = key then List.nth elt target_col :: acc
        else acc)
      [] file
  with Failure _ ->
    raise (Failure "Attempting to match to column out of range many_matches")

let search_csv_helper_whole_row filename match_col key start_col =
  try
    let file = Csv.load filename in
    match List.find_opt (fun row -> List.nth row match_col = key) file with
    | Some row -> List.filteri (fun i _ -> i >= start_col) row
    | None -> failwith "none found"
  with Failure _ ->
    raise
      (Failure
         ("Attempting to match to column out of range whole_row " ^ filename
        ^ " " ^ key))

let search_csv_helper_many_rows filename match_col key =
  try
    let file = Csv.load filename in
    List.filter (fun row -> List.nth row match_col = key) file
  with Failure _ ->
    raise (Failure "Attempting to match to column out of range many_rows")

let get_multipliers_by_nature n =
  let multipliers =
    search_csv_helper_whole_row "data/multipliers_by_nature.csv" 0 n 1
  in
  List.map float_of_string multipliers

let get_multipliers_by_ailment a =
  try
    let multipliers =
      search_csv_helper_whole_row "data/multipliers_by_ailment.csv" 0 a 1
    in
    List.map float_of_string multipliers
  with Failure _ -> [ 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0 ]

let get_multipliers_by_stat_stages stat_stages =
  let multiplier_by_stat_stage ss =
    search_csv_helper_one_match "data/multipliers_by_stat_stages.csv" 0
      (string_of_int ss) 1
    |> float_of_string
  in
  List.map multiplier_by_stat_stage (stats_to_list stat_stages)

let rec apply_multipliers_list cur_stats multipliers =
  let apply_multipliers cur_stats mult_list =
    let apply_multiplier stat multiplier =
      int_of_float (float_of_int stat *. multiplier)
    in
    List.map2 apply_multiplier (stats_to_list cur_stats) mult_list
    |> list_to_stats
  in
  match multipliers with
  | [] -> cur_stats
  | h :: t -> apply_multipliers_list (apply_multipliers cur_stats h) t

let calc_current_stats base_stats nature level ailment stat_stages =
  let stat_aux base_stat level = ((2 * base_stat) + 52) * level / 100 in
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
    [
      get_multipliers_by_nature nature;
      get_multipliers_by_ailment ailment;
      get_multipliers_by_stat_stages stat_stages;
    ]

let calc_cur_stats p =
  calc_current_stats p.base_stats p.nature p.level p.ailment p.stat_stages

let update_current_stats p = { p with cur_stats = calc_cur_stats p }

let get_pokemon_id poke_name =
  search_csv_helper_one_match "data/pokemon.csv" 1 poke_name 0 |> int_of_string

let get_move_ids pokemon_id =
  let strlst =
    search_csv_helper_many_matches "data/pokemon_moves.csv" 0
      (string_of_int pokemon_id) 2
  in
  List.map int_of_string strlst

let get_type_name type_id =
  search_csv_helper_one_match "data/types.csv" 0 (string_of_int type_id) 1

let get_move_id_from_name move_name =
  let id = search_csv_helper_one_match "data/moves.csv" 1 move_name 0 in
  int_of_string id

let get_move_id_from_move = function
  | {
      id;
      name;
      tipe;
      power;
      pp;
      accuracy;
      priority;
      target;
      damage_class;
      effect_id;
      effect_chance;
    } -> id

let damage_class_to_string damage_class =
  match damage_class with
  | Physical -> "Physical"
  | Special -> "Special"
  | Status -> "Status"

let damage_class_from_int i =
  match i with
  | 1 -> Physical
  | 2 -> Special
  | 3 -> Status
  | _ -> failwith "Unknown"

let move_to_string move =
  move.name ^ " (Type: " ^ move.tipe ^ ", Power: " ^ string_of_int move.power
  ^ ", PP: " ^ string_of_int move.pp ^ ", Accuracy: "
  ^ string_of_int move.accuracy
  ^ ", Damage Class: "
  ^ damage_class_to_string move.damage_class
  ^ ")"

let pokemon_to_string pokemon =
  let moves =
    if List.length pokemon.moves = 0 then "No moves available."
    else pokemon.moves |> List.map move_to_string |> String.concat "\n"
  in
  pokemon.species ^ ": " ^ moves

let create_move_from_name move_name =
  match search_csv_helper_whole_row "data/moves.csv" 1 move_name 0 with
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
          (if target_id = "10" then Enemy
           else if target_id = "11" then Self
           else failwith "Not 10 or 11");
        damage_class = damage_class_from_int (int_of_string damage_class_id);
        effect_id = int_of_string effect_id;
        effect_chance = int_of_string effect_chance;
      }
  | _ -> failwith "Invalid move ID"

let get_moves pokemon_species =
  let poke_id = get_pokemon_id pokemon_species in
  let move_ids = get_move_ids poke_id in
  let find_identifier move_id =
    search_csv_helper_one_match "data/moves.csv" 0 (string_of_int move_id) 1
  in
  List.map find_identifier move_ids

let display_learnable_moves pokemon_species =
  let poke_id = get_pokemon_id pokemon_species in
  let move_ids = get_move_ids poke_id in
  let rec find_details move_id =
    match
      search_csv_helper_whole_row "data/moves.csv" 0 (string_of_int move_id) 0
    with
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
    ] ->
        let move_type = get_type_name (int_of_string type_id) in
        let damage_class =
          damage_class_from_int (int_of_string damage_class_id)
        in
        Printf.sprintf
          "%s (Type: %s, Power: %s, PP: %s, Accuracy: %s, Damage Class: %s)"
          identifier move_type power pp accuracy
          (damage_class_to_string damage_class)
    | _ -> "Unknown move"
  in
  print_endline ("Learnable moves for " ^ pokemon_species ^ ":");
  List.iter
    (fun move_id ->
      let move_details = find_details move_id in
      print_endline ("- " ^ move_details))
    move_ids

let create name lvl nat =
  let pokemon_id = get_pokemon_id name in
  let tipes =
    search_csv_helper_many_rows "data/pokemon_types.csv" 0
      (string_of_int pokemon_id)
  in

  let tipe =
    let match_tipe tipe_id =
      search_csv_helper_one_match "data/types.csv" 0 (string_of_int tipe_id) 1
    in

    let first_tipe = match_tipe (int_of_string (List.nth (List.hd tipes) 1)) in
    if List.length tipes = 1 then (first_tipe, "NoneType")
    else
      let second_tipe =
        match_tipe (int_of_string (List.nth (List.nth tipes 1) 1))
      in
      (first_tipe, second_tipe)
  in

  let stat_list =
    search_csv_helper_many_rows "data/pokemon_stats.csv" 0
      (string_of_int pokemon_id)
  in

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
    ignore
      (search_csv_helper_one_match "data/multipliers_by_nature.csv" 0 nat 0);
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
    match
      List.find_opt
        (fun row -> List.hd row = move_type && List.nth row 1 = target_type)
        (Csv.load "data/effectiveness_by_type.csv")
    with
    | Some row -> float_of_string (List.nth row 2)
    | None -> 1.0
  in

  let move_type = move.tipe in
  let type1, type2 = defender.tipe in

  let eff1 = multiplier move_type type1 in
  let eff2 = if type2 = "NoneType" then 1.0 else multiplier move_type type2 in

  eff1 *. eff2

let () = Random.self_init ()
let roll chance = Random.int 100 < chance

let calc_damage attacker defender move =
  let a_stat, d_stat =
    match move.damage_class with
    | Physical ->
        let attack_stat = attacker.cur_stats.atk in
        let defense_stat = defender.cur_stats.def in
        (attack_stat, defense_stat)
    | Special ->
        let attack_stat = attacker.cur_stats.spatk in
        let defense_stat = defender.cur_stats.spdef in
        (attack_stat, defense_stat)
    | Status -> failwith "A move damage class is invalid"
  in

  (*for now just use stage 1*)
  let calculate_crit = Random.int 16 in
  let crit = if calculate_crit = 0 then 1.5 else 1. in
  let rand = float_of_int (85 + Random.int 16) /. 100.0 in
  (*"same type attack bonus" or STAB*)
  let stab =
    match attacker.tipe with
    | t1, t2 -> if t1 = move.tipe || t2 = move.tipe then 1.5 else 1.
  in
  let eff_mult = calc_effectiveness_mult move defender in

  int_of_float
    ((((2.0 *. float_of_int attacker.level /. 5.0) +. 2.0)
      *. float_of_int move.power
      *. (float_of_int a_stat /. float_of_int d_stat)
      /. 50.0
     +. 2.0)
    *. crit *. rand *. stab *. eff_mult)

let deal_damage attacker defender move =
  let damage = calc_damage attacker defender move in
  print_endline ("Dealt " ^ string_of_int damage ^ " damage.");

  defender.cur_hp <- max 0 (defender.cur_hp - damage);
  (attacker, defender)

(* Applies the effect of the move to the target. Calls deal_damage after if
   needed*)
let apply_effect (attacker : t) defender move =
  let a, d =
    if move.effect_chance = -1 || roll move.effect_chance then
      let target = if move.target = Self then attacker else defender in
      let target =
        match move.effect_id with
        | 1 -> (* nothing *) target
        | _ -> failwith "Unknown effect ID"
      in
      if move.target = Self then (target, defender) else (attacker, target)
    else (attacker, defender)
  in
  if move.effect_chance = -1 then (a, d) else deal_damage a d move

let attack attacker defender move =
  let attacker_move = List.find (fun m -> m = move) attacker.moves in
  if attacker_move.pp = 0 then (
    print_endline "Out of pp!";
    (attacker, defender))
  else
    let updated_pp_move = { move with pp = move.pp - 1 } in
    let updated_moves =
      List.map
        (fun m -> if m.name = move.name then updated_pp_move else m)
        attacker.moves
    in
    let updated_attacker = { attacker with moves = updated_moves } in

    if move.effect_id = 1 then
      let new_attacker, new_defender =
        deal_damage updated_attacker defender move
      in
      (new_attacker, new_defender)
    else apply_effect updated_attacker defender move

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

let add_pokemon_move pokemon move_name =
  let new_move = create_move_from_name move_name in
  if List.length pokemon.moves >= 4 then
    raise (Failure "A Pokemon can only have up to 4 moves.")
  else { pokemon with moves = pokemon.moves @ [ new_move ] }
