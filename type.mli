type type_expr = Integer | Boolean | Array of type_expr;;

type var_list = (string * type_expr) list
type program = {
  (* variables globales *)
  global_vars : var_list;
  (* procédures et fonctions *)
  definitions : (string * definition) list;
  (* corps du programme *)
  main : instruction; }

and definition = {
  (* arguments (avec leurs types) *)
  arguments : var_list;
  (* type du résultat (None pour une procédure) *)
  result : type_expr option;
  (* variables locales *)
  local_vars : var_list;
  (* corps de la fonction *)
  body : instruction;  }

and expression =
  | Int of int | Bool of bool
  | Neg of expression
  | Not of expression
  | Bin of binop * expression * expression
  | Get of string
  | Function_call of string * expression list
  | Geti of expression * expression
  | Alloc of expression * type_expr

and binop =
  | Plus | Minus | Times | Div
  | Lt | Le | Gt | Ge | Eq | Ne
  | And | Or  

and instruction =
  | Set of string * expression
  | Sequence of instruction list
  | If of expression * instruction * instruction
  | While of expression * instruction
  | Procedure_call of string * expression list
  | Write_int of expression
  | Writeln_int of expression
  | Read_int of expression
  | Seti of expression * expression * expression