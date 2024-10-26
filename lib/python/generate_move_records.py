import pandas as pd

# Load the moves and move_damage_classes CSVs
moves = pd.read_csv("data/moves.csv")
move_damage_classes = pd.read_csv("data/move_damage_classes.csv")

# Define a mapping from damage class IDs to OCaml variants
damage_class_mapping = {
    1: "Status",
    2: "Physical",
    3: "Special"
}

# Define the target mapping based on the problem's rules
def map_target(target_id):
    return "Self" if target_id in [3, 4, 5, 7, 13, 15] else "Enemy"

# Define a mapping from type IDs to OCaml variants
def map_type_id_to_variant(type_id):
    tipe_mapping = {
        1: "Normal", 2: "Fighting", 3: "Flying", 4: "Poison",
        5: "Ground", 6: "Rock", 7: "Bug", 8: "Ghost", 9: "Steel",
        10: "Fire", 11: "Water", 12: "Grass", 13: "Electric",
        14: "Psychic", 15: "Ice", 16: "Dragon", 17: "Dark", 18: "Fairy"
    }
    return tipe_mapping.get(type_id, "NoneType")

# Helper function to format move names with spaces and capitalize each word
def format_move_name(name):
    return " ".join([word.capitalize() for word in name.split("-")])

# Generate OCaml `let` statements for all moves in the dataset
ocaml_moves = []
for _, move in moves.iterrows():
    move_name = move['identifier'].replace("-", "_")  # Replace dashes with underscores for valid OCaml identifiers
    formatted_name = format_move_name(move['identifier'])  # Capitalize and replace dashes with spaces
    move_record = (
        f"let {move_name} = {{\n"
        f"  id = {move['id']};\n"
        f"  name = \"{formatted_name}\";\n"
        f"  tipe = {map_type_id_to_variant(move['type_id'])};\n"
        f"  power = {int(move['power']) if not pd.isna(move['power']) else 0};\n"
        f"  pp = {int(move['pp']) if not pd.isna(move['pp']) else 0};\n"
        f"  accuracy = {int(move['accuracy']) if not pd.isna(move['accuracy']) else 0};\n"
        f"  priority = {int(move['priority'])};\n"
        f"  target = {map_target(move['target_id'])};\n"
        f"  damage_class = {damage_class_mapping[move['damage_class_id']]};\n"
        f"  effect_id = {int(move['effect_id']) if not pd.isna(move['effect_id']) else 0};\n"
        f"  effect_chance = {int(move['effect_chance']) if not pd.isna(move['effect_chance']) else 0};\n"
        f"}}\n"
    )
    ocaml_moves.append(move_record)

# Combine the generated OCaml code into individual let statements
ocaml_code = "\n".join(ocaml_moves)

# Output the generated OCaml code
text_file = open("move_records_output.txt", "w")
text_file.write(ocaml_code)
text_file.close()