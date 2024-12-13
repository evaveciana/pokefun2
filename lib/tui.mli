val display_pokemon : int -> unit
(**Function to display the ASCII art of a specific Pokémon by its ID *)

val chunk_string : string -> int -> string list
(** Helper function to split a string into chunks of a specified length *)

val load_ascii_csv : string -> string list list
(** Function to load and parse the CSV file containing ASCII art *)

val print_pokemon_in_ascii : string -> int -> unit
(** Function to print a specific Pokémon in ASCII art *)
