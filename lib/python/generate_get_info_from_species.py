import pandas as pd

# Load all the provided CSVs
pokemon = pd.read_csv("data/pokemon.csv")
pokemon_types = pd.read_csv("data/pokemon_types.csv")
pokemon_stats = pd.read_csv("data/pokemon_stats.csv")
pokemon_moves = pd.read_csv("data/pokemon_moves.csv")
types = pd.read_csv("data/types.csv")
stats = pd.read_csv("data/stats.csv")
moves = pd.read_csv("data/moves.csv")
abilities = pd.read_csv("data/abilities.csv")
pokemon_abilities = pd.read_csv("data/pokemon_abilities.csv")

# Create a dictionary mapping from Pokémon IDs to their names
id_to_name = pokemon.set_index('id')['identifier'].to_dict()

# Helper function to convert type list into a tuple with NoneType handling
def get_type_tuple(types_list):
    if len(types_list) == 1:
        return (types_list[0].capitalize(), 'NoneType')
    return (types_list[0].capitalize(), types_list[1].capitalize())

# Prepare type information for all Pokémon
pokemon_types_joined = pd.merge(
    pokemon_types, types[['id', 'identifier']],
    left_on='type_id', right_on='id', how='inner'
).drop(columns=['id', 'type_id'])

grouped_types = pokemon_types_joined.groupby('pokemon_id')['identifier'].apply(list).to_dict()
pokemon_types_dict = {id_to_name[poke_id]: get_type_tuple(types_list) for poke_id, types_list in grouped_types.items()}

# Prepare stats for all Pokémon
stats_joined = pd.merge(
    pokemon_stats, stats[['id', 'identifier']],
    left_on='stat_id', right_on='id', how='inner'
).drop(columns=['id', 'stat_id'])

pivoted_stats = stats_joined.pivot(index='pokemon_id', columns='identifier', values='base_stat').reset_index()
pivoted_stats_dict = pivoted_stats.set_index('pokemon_id').to_dict('index')

pokemon_stats_dict = {
    id_to_name[poke_id]: {
        'hp': stats_dict['hp'], 'atk': stats_dict['atk'], 'def': stats_dict['def'],
        'spatk': stats_dict['spatk'], 'spdef': stats_dict['spdef'], 'spd': stats_dict['spd']
    }
    for poke_id, stats_dict in pivoted_stats_dict.items()
}

# Extract all Pokémon moves
pokemon_moves_joined = pd.merge(
    pokemon_moves, moves[['id', 'identifier']], 
    left_on='move_id', right_on='id', how='inner'
)
grouped_all_moves = pokemon_moves_joined.groupby('pokemon_id')['identifier'].apply(list).to_dict()
pokemon_all_moves_dict = {id_to_name[poke_id]: moves for poke_id, moves in grouped_all_moves.items()}

# Extract abilities for all Pokémon
pokemon_abilities_joined = pd.merge(
    pokemon_abilities, abilities[['id', 'identifier']],
    left_on='ability_id', right_on='id', how='inner'
).drop(columns=['id', 'ability_id'])

grouped_abilities = pokemon_abilities_joined.groupby('pokemon_id')['identifier'].apply(list).to_dict()
pokemon_abilities_dict = {id_to_name[poke_id]: abilities for poke_id, abilities in grouped_abilities.items()}

# Collect all data into a structure for OCaml code generation
all_pokemon_data = {
    name: {
        'tipe': pokemon_types_dict.get(name, ('NoneType', 'NoneType')),
        'stats': pokemon_stats_dict[name],
        'moves': pokemon_all_moves_dict.get(name, []),
        'abilities': pokemon_abilities_dict.get(name, [])
    }
    for name in id_to_name.values()
}

# Generate OCaml code for each Pokémon entry with abilities
ocaml_code = []
for name, data in all_pokemon_data.items():
    tipe = f"({data['tipe'][0]}, {data['tipe'][1]})"
    stats = data['stats']
    stats_str = (f"{{ hp = {stats['hp']}; atk = {stats['atk']}; def = {stats['def']}; "
                 f"spatk = {stats['spatk']}; spdef = {stats['spdef']}; spd = {stats['spd']} }}")
    
    # Replace dashes with underscores and remove duplicates in move names
    unique_moves = list(set([move.replace("-", "_") for move in data['moves']]))
    moves = "; ".join(unique_moves)
    
    abilities = "; ".join([f'"{ability}"' for ability in data['abilities']])  # Add quotes around abilities
    
    entry = (
        f'  | "{name}" ->\n'
        f'      {{ tipe = {tipe};\n'
        f'        stats = {stats_str};\n'
        f'        moves = [{moves}];\n'
        f'        possible_abilities = [{abilities}] }}'
    )
    ocaml_code.append(entry)

# Combine all entries into the final function
final_code = (
    "let get_info_from_species species =\n"
    "  match species with\n" + "\n".join(ocaml_code) + "\n  | _ -> failwith \"Unknown Pokemon\""
)

# Output the final OCaml code
text_file = open("get_info_output.txt", "w")
text_file.write(final_code)
text_file.close()