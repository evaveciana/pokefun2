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
    tipe = (Water, NoneType);
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

let get_tipe_from_species species =
  match species with
  | "Bulbasaur" -> (Grass, Poison)
  | "Ivysaur" -> (Grass, Poison)
  | "Venusaur" -> (Grass, Poison)
  | "Charmander" -> (Fire, NoneType)
  | "Charmeleon" -> (Fire, NoneType)
  | "Charizard" -> (Fire, Flying)
  | "Squirtle" -> (Water, NoneType)
  | "Wartortle" -> (Water, NoneType)
  | "Blastoise" -> (Water, NoneType)
  | "Caterpie" -> (Bug, NoneType)
  | "Metapod" -> (Bug, NoneType)
  | "Butterfree" -> (Bug, Flying)
  | "Weedle" -> (Bug, Poison)
  | "Kakuna" -> (Bug, Poison)
  | "Beedrill" -> (Bug, Poison)
  | "Pidgey" -> (Normal, Flying)
  | "Pidgeotto" -> (Normal, Flying)
  | "Pidgeot" -> (Normal, Flying)
  | "Rattata" -> (Normal, NoneType)
  | "Raticate" -> (Normal, NoneType)
  | "Spearow" -> (Normal, Flying)
  | "Fearow" -> (Normal, Flying)
  | "Ekans" -> (Poison, NoneType)
  | "Arbok" -> (Poison, NoneType)
  | "Pikachu" -> (Electric, NoneType)
  | "Raichu" -> (Electric, NoneType)
  | "Sandshrew" -> (Ground, NoneType)
  | "Sandslash" -> (Ground, NoneType)
  | "Nidoran♀" -> (Poison, NoneType)
  | "Nidorina" -> (Poison, NoneType)
  | "Nidoqueen" -> (Poison, Ground)
  | "Nidoran♂" -> (Poison, NoneType)
  | "Nidorino" -> (Poison, NoneType)
  | "Nidoking" -> (Poison, Ground)
  | "Clefairy" -> (Fairy, NoneType)
  | "Clefable" -> (Fairy, NoneType)
  | "Vulpix" -> (Fire, NoneType)
  | "Ninetales" -> (Fire, NoneType)
  | "Jigglypuff" -> (Normal, Fairy)
  | "Wigglytuff" -> (Normal, Fairy)
  | "Zubat" -> (Poison, Flying)
  | "Golbat" -> (Poison, Flying)
  | "Oddish" -> (Grass, Poison)
  | "Gloom" -> (Grass, Poison)
  | "Vileplume" -> (Grass, Poison)
  | "Paras" -> (Bug, Grass)
  | "Parasect" -> (Bug, Grass)
  | "Venonat" -> (Bug, Poison)
  | "Venomoth" -> (Bug, Poison)
  | "Diglett" -> (Ground, NoneType)
  | "Dugtrio" -> (Ground, NoneType)
  | "Meowth" -> (Normal, NoneType)
  | "Persian" -> (Normal, NoneType)
  | "Psyduck" -> (Water, NoneType)
  | "Golduck" -> (Water, NoneType)
  | "Mankey" -> (Fighting, NoneType)
  | "Primeape" -> (Fighting, NoneType)
  | "Growlithe" -> (Fire, NoneType)
  | "Arcanine" -> (Fire, NoneType)
  | "Poliwag" -> (Water, NoneType)
  | "Poliwhirl" -> (Water, NoneType)
  | "Poliwrath" -> (Water, Fighting)
  | "Abra" -> (Psychic, NoneType)
  | "Kadabra" -> (Psychic, NoneType)
  | "Alakazam" -> (Psychic, NoneType)
  | "Machop" -> (Fighting, NoneType)
  | "Machoke" -> (Fighting, NoneType)
  | "Machamp" -> (Fighting, NoneType)
  | "Bellsprout" -> (Grass, Poison)
  | "Weepinbell" -> (Grass, Poison)
  | "Victreebel" -> (Grass, Poison)
  | "Tentacool" -> (Water, Poison)
  | "Tentacruel" -> (Water, Poison)
  | "Geodude" -> (Rock, Ground)
  | "Graveler" -> (Rock, Ground)
  | "Golem" -> (Rock, Ground)
  | "Ponyta" -> (Fire, NoneType)
  | "Rapidash" -> (Fire, NoneType)
  | "Slowpoke" -> (Water, Psychic)
  | "Slowbro" -> (Water, Psychic)
  | "Magnemite" -> (Electric, NoneType)
  | "Magneton" -> (Electric, NoneType)
  | "Farfetch'd" -> (Normal, Flying)
  | "Doduo" -> (Normal, Flying)
  | "Dodrio" -> (Normal, Flying)
  | "Seel" -> (Water, NoneType)
  | "Dewgong" -> (Water, Ice)
  | "Grimer" -> (Poison, NoneType)
  | "Muk" -> (Poison, NoneType)
  | "Shellder" -> (Water, NoneType)
  | "Cloyster" -> (Water, Ice)
  | "Gastly" -> (Ghost, Poison)
  | "Haunter" -> (Ghost, Poison)
  | "Gengar" -> (Ghost, Poison)
  | "Onix" -> (Rock, Ground)
  | "Drowzee" -> (Psychic, NoneType)
  | "Hypno" -> (Psychic, NoneType)
  | "Krabby" -> (Water, NoneType)
  | "Kingler" -> (Water, NoneType)
  | "Voltorb" -> (Electric, NoneType)
  | "Electrode" -> (Electric, NoneType)
  | "Exeggcute" -> (Grass, Psychic)
  | "Exeggutor" -> (Grass, Psychic)
  | "Cubone" -> (Ground, NoneType)
  | "Marowak" -> (Ground, NoneType)
  | "Hitmonlee" -> (Fighting, NoneType)
  | "Hitmonchan" -> (Fighting, NoneType)
  | "Lickitung" -> (Normal, NoneType)
  | "Koffing" -> (Poison, NoneType)
  | "Weezing" -> (Poison, NoneType)
  | "Rhyhorn" -> (Ground, Rock)
  | "Rhydon" -> (Ground, Rock)
  | "Chansey" -> (Normal, NoneType)
  | "Tangela" -> (Grass, NoneType)
  | "Kangaskhan" -> (Normal, NoneType)
  | "Horsea" -> (Water, NoneType)
  | "Seadra" -> (Water, NoneType)
  | "Goldeen" -> (Water, NoneType)
  | "Seaking" -> (Water, NoneType)
  | "Staryu" -> (Water, NoneType)
  | "Starmie" -> (Water, Psychic)
  | "Mr. Mime" -> (Psychic, Fairy)
  | "Scyther" -> (Bug, Flying)
  | "Jynx" -> (Ice, Psychic)
  | "Electabuzz" -> (Electric, NoneType)
  | "Magmar" -> (Fire, NoneType)
  | "Pinsir" -> (Bug, NoneType)
  | "Tauros" -> (Normal, NoneType)
  | "Magikarp" -> (Water, NoneType)
  | "Gyarados" -> (Water, Flying)
  | "Lapras" -> (Water, Ice)
  | "Ditto" -> (Normal, NoneType)
  | "Eevee" -> (Normal, NoneType)
  | "Vaporeon" -> (Water, NoneType)
  | "Jolteon" -> (Electric, NoneType)
  | "Flareon" -> (Fire, NoneType)
  | "Porygon" -> (Normal, NoneType)
  | "Omanyte" -> (Rock, Water)
  | "Omastar" -> (Rock, Water)
  | "Kabuto" -> (Rock, Water)
  | "Kabutops" -> (Rock, Water)
  | "Aerodactyl" -> (Rock, Flying)
  | "Snorlax" -> (Normal, NoneType)
  | "Articuno" -> (Ice, Flying)
  | "Zapdos" -> (Electric, Flying)
  | "Moltres" -> (Fire, Flying)
  | "Dratini" -> (Dragon, NoneType)
  | "Dragonair" -> (Dragon, NoneType)
  | "Dragonite" -> (Dragon, Flying)
  | "Mewtwo" -> (Psychic, NoneType)
  | "Mew" -> (Psychic, NoneType)
  | "Chikorita" -> (Grass, NoneType)
  | "Bayleef" -> (Grass, NoneType)
  | "Meganium" -> (Grass, NoneType)
  | "Cyndaquil" -> (Fire, NoneType)
  | "Quilava" -> (Fire, NoneType)
  | "Typhlosion" -> (Fire, NoneType)
  | "Totodile" -> (Water, NoneType)
  | "Croconaw" -> (Water, NoneType)
  | "Feraligatr" -> (Water, NoneType)
  | "Sentret" -> (Normal, NoneType)
  | "Furret" -> (Normal, NoneType)
  | "Hoothoot" -> (Normal, Flying)
  | "Noctowl" -> (Normal, Flying)
  | "Ledyba" -> (Bug, Flying)
  | "Ledian" -> (Bug, Flying)
  | "Spinarak" -> (Bug, Poison)
  | "Ariados" -> (Bug, Poison)
  | "Crobat" -> (Poison, Flying)
  | "Chinchou" -> (Water, Electric)
  | "Lanturn" -> (Water, Electric)
  | "Pichu" -> (Electric, NoneType)
  | "Cleffa" -> (Fairy, NoneType)
  | "Igglybuff" -> (Normal, Fairy)
  | "Togepi" -> (Fairy, NoneType)
  | "Togetic" -> (Fairy, Flying)
  | "Natu" -> (Psychic, Flying)
  | "Xatu" -> (Psychic, Flying)
  | "Mareep" -> (Electric, NoneType)
  | "Flaaffy" -> (Electric, NoneType)
  | "Ampharos" -> (Electric, NoneType)
  | "Bellossom" -> (Grass, NoneType)
  | "Marill" -> (Water, Fairy)
  | "Azumarill" -> (Water, Fairy)
  | "Sudowoodo" -> (Rock, NoneType)
  | "Politoed" -> (Water, NoneType)
  | "Hoppip" -> (Grass, Flying)
  | "Skiploom" -> (Grass, Flying)
  | "Jumpluff" -> (Grass, Flying)
  | "Aipom" -> (Normal, NoneType)
  | "Sunkern" -> (Grass, NoneType)
  | "Sunflora" -> (Grass, NoneType)
  | "Yanma" -> (Bug, Flying)
  | "Wooper" -> (Water, Ground)
  | "Quagsire" -> (Water, Ground)
  | "Espeon" -> (Psychic, NoneType)
  | "Umbreon" -> (Dark, NoneType)
  | "Murkrow" -> (Dark, Flying)
  | "Slowking" -> (Water, Psychic)
  | "Misdreavus" -> (Ghost, NoneType)
  | "Unown" -> (Psychic, NoneType)
  | "Wobbuffet" -> (Psychic, NoneType)
  | "Girafarig" -> (Normal, Psychic)
  | "Pineco" -> (Bug, NoneType)
  | "Forretress" -> (Bug, Steel)
  | "Dunsparce" -> (Normal, NoneType)
  | "Gligar" -> (Ground, Flying)
  | "Steelix" -> (Steel, Ground)
  | "Snubbull" -> (Fairy, NoneType)
  | "Granbull" -> (Fairy, NoneType)
  | "Qwilfish" -> (Water, Poison)
  | "Scizor" -> (Bug, Steel)
  | "Shuckle" -> (Bug, Rock)
  | "Heracross" -> (Bug, Fighting)
  | "Sneasel" -> (Dark, Ice)
  | "Teddiursa" -> (Normal, NoneType)
  | "Ursaring" -> (Normal, NoneType)
  | "Slugma" -> (Fire, NoneType)
  | "Magcargo" -> (Fire, Rock)
  | "Swinub" -> (Ice, Ground)
  | "Piloswine" -> (Ice, Ground)
  | "Corsola" -> (Water, Rock)
  | "Remoraid" -> (Water, NoneType)
  | "Octillery" -> (Water, NoneType)
  | "Delibird" -> (Ice, Flying)
  | "Mantine" -> (Water, Flying)
  | "Skarmory" -> (Steel, Flying)
  | "Houndour" -> (Dark, Fire)
  | "Houndoom" -> (Dark, Fire)
  | "Kingdra" -> (Water, Dragon)
  | "Phanpy" -> (Ground, NoneType)
  | "Donphan" -> (Ground, NoneType)
  | "Porygon2" -> (Normal, NoneType)
  | "Stantler" -> (Normal, NoneType)
  | "Smeargle" -> (Normal, NoneType)
  | "Tyrogue" -> (Fighting, NoneType)
  | "Hitmontop" -> (Fighting, NoneType)
  | "Smoochum" -> (Ice, Psychic)
  | "Elekid" -> (Electric, NoneType)
  | "Magby" -> (Fire, NoneType)
  | "Miltank" -> (Normal, NoneType)
  | "Blissey" -> (Normal, NoneType)
  | "Raikou" -> (Electric, NoneType)
  | "Entei" -> (Fire, NoneType)
  | "Suicune" -> (Water, NoneType)
  | "Larvitar" -> (Rock, Ground)
  | "Pupitar" -> (Rock, Ground)
  | "Tyranitar" -> (Rock, Dark)
  | "Lugia" -> (Psychic, Flying)
  | "Ho-oh" -> (Fire, Flying)
  | "Celebi" -> (Psychic, Grass)
  | _ -> (Normal, NoneType)

let get_stats_from_species species = zero_stats
let calc_stats base_stats nature level = zero_stats
