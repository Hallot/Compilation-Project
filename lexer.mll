{
  open Lexing
  open Parser
  exception Eof
}

rule token = parse
  | [ ' ' '\t' '\n' ]+ {token lexbuf}
  | "true" {TRUE}
  | "false" {FALSE}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {SLASH}
  | "and" {AND}
  | "or" {OR}
  | "not" {NOT}
  | "<" {LT}
  | "<=" {LE}
  | ">" {GT}
  | ">=" {GE}
  | "=" {EQ}
  | "<>" {NE}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "," {COMMA}
  | ":=" {COLONEQ}
  | ";" {SEMICOLON}
  | ":" {COLON}
  | "." {DOT}
  | "program" {PROGRAM}
  | "begin" {BEGIN}
  | "end" {END}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "while" {WHILE}
  | "do" {DO}
  | "procedure" {PROCEDURE}
  | "function" {FUNCTION}
  | "var" {VAR}
  | "readln" {READLN}
  | "write" {WRITE}
  | "writeln" {WRITELN}
  | "integer" {INTEGER}
  | "boolean" {BOOLEAN}
  | "new" {NEW}
  | "array" {ARRAY}
  | "of" {OF}
  | '{' {comment lexbuf}
  | ((['A'-'Z' 'a'-'z']+['0'-'9']*) as id) {IDENTIFIER id}
  | (['0'-'9']+ as i) {INTCONST (int_of_string i)}
  | _ as c {print_char c; token lexbuf}
  | eof {Printf.printf("end of file"); EOF}

and comment = parse
  | '}' {token lexbuf}
  | _ {comment lexbuf}

{
}
