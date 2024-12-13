(* Helper function to split a string into chunks of a specified length *)
let chunk_string str chunk_size =
  let length = String.length str in
  let rec aux i acc =
    if i >= length then List.rev acc
    else
      let chunk =
        if i + chunk_size > length then String.sub str i (length - i)
        else String.sub str i chunk_size
      in
      aux (i + chunk_size) (chunk :: acc)
  in
  aux 0 []

(* Function to load and parse the CSV file containing ASCII art *)
let load_ascii_csv filename =
  let lines = Csv.load filename |> List.concat in
  let rec parse_ascii chunks current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | line :: rest ->
        if line = "" then parse_ascii chunks [] (List.rev current :: acc) rest
        else parse_ascii chunks (line :: current) acc rest
  in
  parse_ascii [] [] [] lines

(* Function to print a specific Pokémon in ASCII art *)
let print_pokemon_in_ascii filename pokemon_index =
  try
    let ascii_art_list = load_ascii_csv filename in
    (* Fetch and print the specified Pokémon's ASCII art *)
    let ascii_art = List.nth ascii_art_list pokemon_index in
    List.iter print_endline ascii_art
  with
  | Failure _ -> print_endline "Invalid Pokémon\n\n   index."
  | _ -> print_endline ("Error loading or parsing ASCII file. " ^ filename)

let display_pokemon pokemon_id =
  let filename = "data/ascii.csv" in
  let csv_index = (2 * (pokemon_id - 1)) + 1 in
  print_pokemon_in_ascii filename csv_index
