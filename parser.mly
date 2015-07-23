%{
        open Type
        open List
%}

%token <int> INTCONST
%token <string> IDENTIFIER
%token PLUS MINUS TIMES SLASH AND OR NOT LT LE GT GE EQ NE TRUE FALSE UMINUS
%token LPAREN RPAREN LBRACKET RBRACKET COMMA COLONEQ SEMICOLON COLON DOT
%token PROGRAM BEGIN END IF THEN ELSE WHILE DO PROCEDURE FUNCTION VAR
%token NEW READLN WRITE WRITELN
%token INTEGER BOOLEAN ARRAY OF EOF
%left   EQ   NE
%left   GT LE LT GE
%left   PLUS    MINUS
%left   TIMES   DIVIDE
%left   UMINUS  NOT
%start program									/* the entry point */
%type <Type.program> program

%%


program : PROGRAM optional_variable definitions main DOT {{global_vars = $2;definitions = $3;main = $4}};

optional_variable : 
        | {[]}
        | variables_with_var {$1}

variables_with_var : VAR variables %prec SEMICOLON {$2}

variables : 
          | {[]}
          | list_variables COLON typ {(map (fun var -> (var,$3)) $1)}
          | list_variables COLON typ SEMICOLON variables {(map (fun var -> (var,$3))$1)@$5}

list_variables : 
               | IDENTIFIER {[$1]} 
               | IDENTIFIER COMMA list_variables {$1::$3}

typ : 
    | INTEGER {Integer} 
    | BOOLEAN {Boolean} 
    | ARRAY OF typ {Array($3)}

definitions : 
            | {[]}
            | definition_optional_semi definitions {$1::$2}

definition_optional_semi : 
          | definition SEMICOLON {$1}
          | definition {$1}
            
definition : 
           | PROCEDURE procedure {$2}
           | FUNCTION fonction {$2}

procedure : IDENTIFIER arguments SEMICOLON optional_variable body
  {($1,{arguments = $2;result = None; local_vars = $4; body = $5})}

fonction : IDENTIFIER arguments COLON typ SEMICOLON optional_variable body
  {($1,{arguments = $2;result = Some($4); local_vars = $6; body = $7})}

arguments : LPAREN variables RPAREN {$2}

body : instruction_ {$1}

instruction_ : 
          | instruction SEMICOLON {$1}
          | instruction {$1}

instruction : 
            | IF expression THEN instruction_ ELSE instruction_ {If($2,$4,$6)}
            | WHILE expression DO instruction_ {While($2,$4)}
            | BEGIN instruction_list END {Sequence($2)}
            | IDENTIFIER LPAREN list_expr RPAREN {Procedure_call ($1,$3)}
            | IDENTIFIER COLONEQ expression {Set($1,$3)}
            | expression LBRACKET expression RBRACKET COLONEQ expression {Seti($1,$3,$6)}
            | IDENTIFIER COLONEQ READLN LPAREN RPAREN {Read_int(Get($1))}
            | expression LBRACKET expression RBRACKET COLONEQ READLN LPAREN RPAREN {Read_int(Geti($1,$3))}
            | WRITE LPAREN expression RPAREN {Write_int($3)}
            | WRITELN LPAREN expression RPAREN {Writeln_int($3)}

instruction_list : 
           | {[]} 
           | instruction_ instruction_list {$1::$2}

expression : 
           | INTCONST {Int($1)} 
           | TRUE {Bool(true)} 
           | FALSE {Bool(false)}
           | IDENTIFIER {Get($1)}
           | expression LBRACKET expression RBRACKET {Geti($1,$3)}
           | IDENTIFIER LPAREN list_expr RPAREN {Function_call($1,$3)}
           | LPAREN expression RPAREN {$2}
           | NEW ARRAY OF typ LBRACKET expression RBRACKET {Alloc($6,$4)}
           | NOT expression {Not($2)}
           | MINUS expression %prec UMINUS {Neg($2)}
           | expression binop expression {Bin($2,$1,$3)}

binop : 
      | MINUS {Minus}
      | PLUS {Plus}
      | TIMES {Times}
      | SLASH {Div}
      | LT {Lt}
      | LE {Le}
      | GT {Gt}
      | GE {Ge}
      | EQ {Eq}
      | NE {Ne}
      | AND {And}
      | OR {Or}

list_expr : 
    | {[]} 
    | expression {[$1]} 
    | expression COMMA list_expr {$1::$3}

main : BEGIN instruction_list END {Sequence($2)}
