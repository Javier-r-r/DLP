
open Parsing;;
open Lexing;;

open Lambda;;
open Execute;;
open Parser;;
open Lexer;;

let read_command () = 
  let prompt1 () = print_string ">> " in
  let prompt2 () = print_string "   " in
  prompt1 ();
  let rec aux ls =
    let l = read_line () in
    match (String.rindex_opt l ';' ) with
      Some i ->
        if (try l.[i-1] = ';' with _ -> false)
          then String.concat " " (List.rev ((String.sub l 0 (i-1))::ls))
        else (prompt2 (); aux (l::ls))    
    | None -> prompt2 (); aux (l::ls)
  in 
    aux []
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    flush stdout;
    try
      let c = s token (from_string (read_command ())) in
      loop (execute ctx c)
    with
      | Lexical_error ->
          print_endline "lexical error";
          loop ctx
      | Parse_error ->
          print_endline "syntax error";
          loop ctx
      | Type_error e ->
          print_endline ("type error: " ^ e);
          loop ctx
      | End_of_file ->
          print_endline "...bye!!!"
  in
  loop emptyctx
;;

top_level_loop ()
;;


