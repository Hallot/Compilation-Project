open Type;;
open List;;
open To_dot;;
open Filename;;

exception Error of exn * (int * int * string );;

let parse_error s = print_endline s ;;

let parse_buf_exn lexbuf =
  try
    Parser.program Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
        raise (Error (exn,(line,cnum,tok)))
    end;;

let get_ast fichier = try
        let fd = open_in fichier in 
        let lb = Lexing.from_channel fd in
          parse_buf_exn lb
with exn -> print_endline ("error in "^fichier);raise exn;;

let tests () = 
  let files = Array.to_list (Sys.readdir "tests") in
  let files = filter (fun s -> check_suffix s ".p") files in 
  let f_test file = try
    let fd = open_in file in 
    let lb = Lexing.from_channel fd in
      let _ = parse_buf_exn lb in ""
  with exn -> file in
  let a = map (fun f -> f_test ("tests/"^f)) files in
  let files_with_errors = filter (fun i -> i <> "") a in
    if (length files_with_errors = 0) then print_endline "Reussi a parser tous les fichiers."
    else print_endline "Fichiers contenant des erreurs : ";iter print_endline files_with_errors

let create_dots () = 
  let files = Array.to_list (Sys.readdir "tests") in
  let files = filter (fun s -> check_suffix s ".p") files in 
  let parsed_list = map (fun fichier -> 
    try 
      get_ast ("tests/"^fichier)
    with exn -> {global_vars=[];definitions=[];main=Sequence []}
  ) files in 
  let graph_list = map Graph.make_graph parsed_list in
  let nom_list = map (fun s -> "tests/"^(chop_extension s)^".dot") files in
    iter2 generate_dot nom_list graph_list;;
