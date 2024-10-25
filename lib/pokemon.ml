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
  | "Bulbasaur" -> (Grass, Poison)          (**Start of Gen I*)
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
  | "Chikorita" -> (Grass, NoneType)          (**Start of Gen II*)
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
  | "Treecko" -> (Grass, NoneType)            (**Start of Gen III*)
  | "Grovyle" -> (Grass, NoneType)
  | "Sceptile" -> (Grass, NoneType)
  | "Torchic" -> (Fire, NoneType)
  | "Combusken" -> (Fire, Fighting)
  | "Blaziken" -> (Fire, Fighting)
  | "Mudkip" -> (Water, NoneType)
  | "Marshtomp" -> (Water, Ground)
  | "Swampert" -> (Water, Ground)
  | "Poochyena" -> (Dark, NoneType)
  | "Mightyena" -> (Dark, NoneType)
  | "Zigzagoon" -> (Normal, NoneType)
  | "Linoone" -> (Normal, NoneType)
  | "Wurmple" -> (Bug, NoneType)
  | "Silcoon" -> (Bug, NoneType)
  | "Beautifly" -> (Bug, Flying)
  | "Cascoon" -> (Bug, NoneType)
  | "Dustox" -> (Bug, Poison)
  | "Lotad" -> (Water, Grass)
  | "Lombre" -> (Water, Grass)
  | "Ludicolo" -> (Water, Grass)
  | "Seedot" -> (Grass, NoneType)
  | "Nuzleaf" -> (Grass, Dark)
  | "Shiftry" -> (Grass, Dark)
  | "Taillow" -> (Normal, Flying)
  | "Swellow" -> (Normal, Flying)
  | "Wingull" -> (Water, Flying)
  | "Pelipper" -> (Water, Flying)
  | "Ralts" -> (Psychic, Fairy)
  | "Kirlia" -> (Psychic, Fairy)
  | "Gardevoir" -> (Psychic, Fairy)
  | "Surskit" -> (Bug, Water)
  | "Masquerain" -> (Bug, Flying)
  | "Shroomish" -> (Grass, NoneType)
  | "Breloom" -> (Grass, Fighting)
  | "Slakoth" -> (Normal, NoneType)
  | "Vigoroth" -> (Normal, NoneType)
  | "Slaking" -> (Normal, NoneType)
  | "Nincada" -> (Bug, Ground)
  | "Ninjask" -> (Bug, Flying)
  | "Shedinja" -> (Bug, Ghost)
  | "Whismur" -> (Normal, NoneType)
  | "Loudred" -> (Normal, NoneType)
  | "Exploud" -> (Normal, NoneType)
  | "Makuhita" -> (Fighting, NoneType)
  | "Hariyama" -> (Fighting, NoneType)
  | "Azurill" -> (Normal, Fairy)
  | "Nosepass" -> (Rock, NoneType)
  | "Skitty" -> (Normal, NoneType)
  | "Delcatty" -> (Normal, NoneType)
  | "Sableye" -> (Dark, Ghost)
  | "Mawile" -> (Steel, Fairy)
  | "Aron" -> (Steel, Rock)
  | "Lairon" -> (Steel, Rock)
  | "Aggron" -> (Steel, Rock)
  | "Meditite" -> (Fighting, Psychic)
  | "Medicham" -> (Fighting, Psychic)
  | "Electrike" -> (Electric, NoneType)
  | "Manectric" -> (Electric, NoneType)
  | "Plusle" -> (Electric, NoneType)
  | "Minun" -> (Electric, NoneType)
  | "Volbeat" -> (Bug, NoneType)
  | "Illumise" -> (Bug, NoneType)
  | "Roselia" -> (Grass, Poison)
  | "Gulpin" -> (Poison, NoneType)
  | "Swalot" -> (Poison, NoneType)
  | "Carvanha" -> (Water, Dark)
  | "Sharpedo" -> (Water, Dark)
  | "Wailmer" -> (Water, NoneType)
  | "Wailord" -> (Water, NoneType)
  | "Numel" -> (Fire, Ground)
  | "Camerupt" -> (Fire, Ground)
  | "Torkoal" -> (Fire, NoneType)
  | "Spoink" -> (Psychic, NoneType)
  | "Grumpig" -> (Psychic, NoneType)
  | "Spinda" -> (Normal, NoneType)
  | "Trapinch" -> (Ground, NoneType)
  | "Vibrava" -> (Ground, Dragon)
  | "Flygon" -> (Ground, Dragon)
  | "Cacnea" -> (Grass, NoneType)
  | "Cacturne" -> (Grass, Dark)
  | "Swablu" -> (Normal, Flying)
  | "Altaria" -> (Dragon, Flying)
  | "Zangoose" -> (Normal, NoneType)
  | "Seviper" -> (Poison, NoneType)
  | "Lunatone" -> (Rock, Psychic)
  | "Solrock" -> (Rock, Psychic)
  | "Barboach" -> (Water, Ground)
  | "Whiscash" -> (Water, Ground)
  | "Corphish" -> (Water, NoneType)
  | "Crawdaunt" -> (Water, Dark)
  | "Baltoy" -> (Ground, Psychic)
  | "Claydol" -> (Ground, Psychic)
  | "Lileep" -> (Rock, Grass)
  | "Cradily" -> (Rock, Grass)
  | "Anorith" -> (Rock, Bug)
  | "Armaldo" -> (Rock, Bug)
  | "Feebas" -> (Water, NoneType)
  | "Milotic" -> (Water, NoneType)
  | "Castform" -> (Normal, NoneType)
  | "Kecleon" -> (Normal, NoneType)
  | "Shuppet" -> (Ghost, NoneType)
  | "Banette" -> (Ghost, NoneType)
  | "Duskull" -> (Ghost, NoneType)
  | "Dusclops" -> (Ghost, NoneType)
  | "Tropius" -> (Grass, Flying)
  | "Chimecho" -> (Psychic, NoneType)
  | "Absol" -> (Dark, NoneType)
  | "Wynaut" -> (Psychic, NoneType)
  | "Snorunt" -> (Ice, NoneType)
  | "Glalie" -> (Ice, NoneType)
  | "Spheal" -> (Ice, Water)
  | "Sealeo" -> (Ice, Water)
  | "Walrein" -> (Ice, Water)
  | "Clamperl" -> (Water, NoneType)
  | "Huntail" -> (Water, NoneType)
  | "Gorebyss" -> (Water, NoneType)
  | "Relicanth" -> (Water, Rock)
  | "Luvdisc" -> (Water, NoneType)
  | "Bagon" -> (Dragon, NoneType)
  | "Shelgon" -> (Dragon, NoneType)
  | "Salamence" -> (Dragon, Flying)
  | "Beldum" -> (Steel, Psychic)
  | "Metang" -> (Steel, Psychic)
  | "Metagross" -> (Steel, Psychic)
  | "Regirock" -> (Rock, NoneType)
  | "Regice" -> (Ice, NoneType)
  | "Registeel" -> (Steel, NoneType)
  | "Latias" -> (Dragon, Psychic)
  | "Latios" -> (Dragon, Psychic)
  | "Kyogre" -> (Water, NoneType)
  | "Groudon" -> (Ground, NoneType)
  | "Rayquaza" -> (Dragon, Flying)
  | "Jirachi" -> (Normal, Psychic)
  | "Deoxys (Normal)" -> (Psychic, NoneType)
  | "Deoxys (Attack)" -> (Psychic, NoneType)
  | "Deoxys (Defense)" -> (Psychic, NoneType)
  | "Deoxys (Speed)" -> (Psychic, NoneType)                                          
  | "Turtwig" -> (Grass, NoneType)                (** Start of Gen IV*)
  | "Grotle" -> (Grass, NoneType)
  | "Torterra" -> (Grass, Ground)
  | "Chimchar" -> (Fire, NoneType)
  | "Monferno" -> (Fire, Fighting)
  | "Infernape" -> (Fire, Fighting)
  | "Piplup" -> (Water, NoneType)
  | "Prinplup" -> (Water, NoneType)
  | "Empoleon" -> (Water, Steel)
  | "Starly" -> (Normal, Flying)
  | "Staravia" -> (Normal, Flying)
  | "Staraptor" -> (Normal, Flying)
  | "Bidoof" -> (Normal, NoneType)
  | "Bibarel" -> (Normal, Water)
  | "Kricketot" -> (Bug, NoneType)
  | "Kricketune" -> (Bug, NoneType)
  | "Shinx" -> (Electric, NoneType)
  | "Luxio" -> (Electric, NoneType)
  | "Luxray" -> (Electric, NoneType)
  | "Budew" -> (Grass, Poison)
  | "Roserade" -> (Grass, Poison)
  | "Cranidos" -> (Rock, NoneType)
  | "Rampardos" -> (Rock, NoneType)
  | "Shieldon" -> (Rock, Steel)
  | "Bastiodon" -> (Rock, Steel)
  | "Burmy" -> (Bug, NoneType)
  | "Wormadam" -> (Bug, Grass)
  | "Mothim" -> (Bug, Flying)
  | "Combee" -> (Bug, Flying)
  | "Vespiquen" -> (Bug, Flying)
  | "Pachirisu" -> (Electric, NoneType)
  | "Buizel" -> (Water, NoneType)
  | "Floatzel" -> (Water, NoneType)
  | "Cherubi" -> (Grass, NoneType)
  | "Cherrim" -> (Grass, NoneType)
  | "Shellos" -> (Water, NoneType)
  | "Gastrodon" -> (Water, Ground)
  | "Ambipom" -> (Normal, NoneType)
  | "Drifloon" -> (Ghost, Flying)
  | "Drifblim" -> (Ghost, Flying)
  | "Buneary" -> (Normal, NoneType)
  | "Lopunny" -> (Normal, NoneType)
  | "Mismagius" -> (Ghost, NoneType)
  | "Honchkrow" -> (Dark, Flying)
  | "Glameow" -> (Normal, NoneType)
  | "Purugly" -> (Normal, NoneType)
  | "Chingling" -> (Psychic, NoneType)
  | "Stunky" -> (Poison, Dark)
  | "Skuntank" -> (Poison, Dark)
  | "Bronzor" -> (Steel, Psychic)
  | "Bronzong" -> (Steel, Psychic)
  | "Bonsly" -> (Rock, NoneType)
  | "Mime Jr." -> (Psychic, Fairy)
  | "Happiny" -> (Normal, NoneType)
  | "Chatot" -> (Normal, Flying)
  | "Spiritomb" -> (Ghost, Dark)
  | "Gible" -> (Dragon, Ground)
  | "Gabite" -> (Dragon, Ground)
  | "Garchomp" -> (Dragon, Ground)
  | "Munchlax" -> (Normal, NoneType)
  | "Riolu" -> (Fighting, NoneType)
  | "Lucario" -> (Fighting, Steel)
  | "Hippopotas" -> (Ground, NoneType)
  | "Hippowdon" -> (Ground, NoneType)
  | "Skorupi" -> (Poison, Bug)
  | "Drapion" -> (Poison, Dark)
  | "Croagunk" -> (Poison, Fighting)
  | "Toxicroak" -> (Poison, Fighting)
  | "Carnivine" -> (Grass, NoneType)
  | "Finneon" -> (Water, NoneType)
  | "Lumineon" -> (Water, NoneType)
  | "Mantyke" -> (Water, Flying)
  | "Snover" -> (Grass, Ice)
  | "Abomasnow" -> (Grass, Ice)
  | "Weavile" -> (Dark, Ice)
  | "Magnezone" -> (Electric, Steel)
  | "Lickilicky" -> (Normal, NoneType)
  | "Rhyperior" -> (Ground, Rock)
  | "Tangrowth" -> (Grass, NoneType)
  | "Electivire" -> (Electric, NoneType)
  | "Magmortar" -> (Fire, NoneType)
  | "Togekiss" -> (Fairy, Flying)
  | "Yanmega" -> (Bug, Flying)
  | "Leafeon" -> (Grass, NoneType)
  | "Glaceon" -> (Ice, NoneType)
  | "Gliscor" -> (Ground, Flying)
  | "Mamoswine" -> (Ice, Ground)
  | "Porygon-Z" -> (Normal, NoneType)
  | "Gallade" -> (Psychic, Fighting)
  | "Probopass" -> (Rock, Steel)
  | "Dusknoir" -> (Ghost, NoneType)
  | "Froslass" -> (Ice, Ghost)
  | "Rotom" -> (Electric, Ghost)
  | "Uxie" -> (Psychic, NoneType)
  | "Mesprit" -> (Psychic, NoneType)
  | "Azelf" -> (Psychic, NoneType)
  | "Dialga" -> (Steel, Dragon)
  | "Palkia" -> (Water, Dragon)
  | "Heatran" -> (Fire, Steel)
  | "Regigigas" -> (Normal, NoneType)
  | "Giratina" -> (Ghost, Dragon)
  | "Cresselia" -> (Psychic, NoneType)
  | "Phione" -> (Water, NoneType)
  | "Manaphy" -> (Water, NoneType)
  | "Darkrai" -> (Dark, NoneType)
  | "Shaymin" -> (Grass, NoneType)
  | "Arceus" -> (Normal, NoneType)
  | _ -> (NoneType, NoneType)

let get_stats_from_species species = zero_stats
let calc_stats base_stats nature level = zero_stats
