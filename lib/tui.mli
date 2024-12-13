val display_pokemon : int -> unit
(**[display_pokemon i] dispays the ASCII art of a specific Pokémon by its ID [i] *)

val chunk_string : string -> int -> string list
(** [chunk_string str i] splits a string [str] into chunks of length [i] *)

val load_ascii_csv : string -> string list list
(** [load_ascii_csv str] loads and parses the CSV file at [str] containing ASCII
    art *)

val print_pokemon_in_ascii : string -> int -> unit
(** [print_pokemon_in_ascii str i] prints the pokemon [str] at index [i]. *)
