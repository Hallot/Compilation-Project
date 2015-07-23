open List;;
open Type;;

module Call = Map.Make(String);;

type appel = Terminaux of string | Non_Terminaux of string;;

let rec filter_function_calls_instr = function 
  | Sequence l -> flatten (map filter_function_calls_instr l)
  | If (cond,then_,else_) -> (filter_function_calls_expr cond) @ 
                             (filter_function_calls_instr then_) @
                             (filter_function_calls_instr else_) 
  | While (cond,ex) -> (filter_function_calls_expr cond) @ 
                       (filter_function_calls_instr ex)
  | Procedure_call (proc, exs) -> (Non_Terminaux proc) :: (flatten (map filter_function_calls_expr exs))
  | Writeln_int ex -> (Non_Terminaux "writeln") :: filter_function_calls_expr ex
  | Write_int ex -> (Non_Terminaux "write") :: filter_function_calls_expr ex
  | Read_int ex -> (Non_Terminaux "readln") :: filter_function_calls_expr ex
  | Set (_, ex) -> filter_function_calls_expr ex
  | Seti(arr,i,ex) -> (filter_function_calls_expr arr)@
                      (filter_function_calls_expr i) @
                      (filter_function_calls_expr ex)

and filter_function_calls_expr = function
  | Neg ex | Not ex -> filter_function_calls_expr ex
  | Bin (_,ex1,ex2) -> (filter_function_calls_expr ex1)@
                       (filter_function_calls_expr ex2)
  | Geti (arr,ex) -> (filter_function_calls_expr arr)@
                     (filter_function_calls_expr ex)
  | Alloc (ex,_) -> filter_function_calls_expr ex
  | Function_call (nom,exs) -> (Non_Terminaux nom) :: (flatten (map filter_function_calls_expr exs))
  | _ -> []

let remove_dup l = 
  let f l e = if mem e l then l else e::l in
    fold_left f [] l;;

let rec get_last_instr instr = match instr with
  | Sequence [] -> [] (* may happen in an empty `else` *)
  | Sequence l -> get_last_instr(hd (rev l)) (* get last element *)
  | If(_,then_,else_) -> (get_last_instr then_)@(get_last_instr else_)
  | Procedure_call(nom,_) -> [Terminaux nom]
  | Write_int _ -> [Terminaux "write"]
  | Writeln_int _ -> [Terminaux "writeln"]
  | Read_int _ -> [Terminaux "readln"]
  | Set(_,Function_call(nom,_)) | Seti(_,_,Function_call(nom,_)) -> [Terminaux nom]
  | _ -> [];;

(* Remove all elements of l2 from l1 *)
let rec list_diff l1 l2 = match (l1,l2) with
  | _,[] -> l1
  | [],_ -> failwith "Erreur dans list_diff : terminal non trouve dans la liste des non-terminaux"
  | ((Non_Terminaux a)::t),((Terminaux b)::u) when a = b -> list_diff t u
  | (a::t,_) -> a::(list_diff t l2)
  | _,_ -> failwith "Erreur dans list_diff";;

let add_call calls = function
  |(nom,{arguments = _; result = _; local_vars = _; body = body}) ->
    let terminaux = get_last_instr body in 
    let non_terminaux = filter_function_calls_instr body in
    let final = terminaux@(list_diff non_terminaux terminaux) in
      calls := Call.add nom (remove_dup final) !calls;;

let print_graph calls = 
  let print_nom nom = print_endline ("nom : "^nom) in
  let rec print_list = function
    | [] -> ()
    | (Non_Terminaux a)::tail -> print_endline("-- Non terminal : "^a);
                                 print_list tail
    | (Terminaux a)::tail -> print_endline("-- Terminal : "^a);
                             print_list tail
  in
  let print_assoc nom l = print_nom nom ; print_list l in
    Call.iter print_assoc calls;;
    
let make_graph program = 
  let calls = ref Call.empty in
    begin 
      add_call calls ("program",{arguments=[];result=None;local_vars=[];body=program.main});
      List.iter (add_call calls) program.definitions;
    end;
  !calls
