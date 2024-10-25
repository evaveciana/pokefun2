(*Basic driver program*)
let play () = print_endline "Start playing"
let build_team () = print_endline "Build team"
let show_help_menu () = print_endline "Help menu"

let () =
  print_endline
    "Welcome to Pokemon!\n\
     Press p to start playing with the default team.\n\
     Press t to build a team of your choice.\n\
     Press h for help.";

  let rec choose () =
    let input = read_line () in
    if input = "p" then play ()
    else if input = "t" then build_team ()
    else if input = "h" then show_help_menu ()
    else (
      print_endline "Please press p, t, or h.";
      choose ())
  in
  choose ()
