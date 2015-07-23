open Graph;;
open Type;;
open Printf;;
open Filename;;

(* Transforms '-' into '_'
 * (dot doesn't like graph names containing '-') *)
let sanitize str = 
  let copy = String.copy str in 
    while (String.contains copy '-') do
      let index = String.index copy '-' in
        copy.[index] <- '_'
    done;
    copy;;

let generate_dot output graph = 
  let out = open_out output in
  let rec generate_for_one_node nom = function
    | [] -> []
    | Non_Terminaux (a)::tail -> (nom^" -> "^a^";")::(generate_for_one_node nom tail)
    | Terminaux (a)::tail -> (nom^" -> "^a^" [style=dashed];")::(generate_for_one_node nom tail)
  in
  let nom_sans_extension = sanitize (chop_suffix (basename output) ".dot") in
  let beginning = "digraph "^nom_sans_extension^"{" in
    begin
      fprintf out "%s\n" beginning;
      Call.iter (fun n -> fun l -> 
                   List.iter (fun s -> fprintf out "%s\n" s) (generate_for_one_node n l)
      ) graph;
      fprintf out "%s\n" "}";
      close_out out
    end;;
