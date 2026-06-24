/*
 * SNU 4190.310 Programming Languages 2026 Spring
 * Skeleton for SM5 & SM5 Limited
 */

%{
exception ParsingError of string
%}

%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token PLUS MINUS STAR SLASH EQUAL LB RB NOT COLONEQ SEMICOLON COMMA PERIOD IF THEN ELSE
%token WHILE DO LET IN READ WRITE PROC
%token LP RP LC RC
%token EOF

%nonassoc IN
%left SEMICOLON
%nonassoc DO
%nonassoc ELSE
%right COLONEQ
%right WRITE
%left EQUAL LB
%left PLUS MINUS
%left STAR SLASH
%right NOT
%left PERIOD

%start program
%type <K.exp> program

%%

program:
       expr EOF { $1 }
    ;

expr:
    LP expr RP { $2 }
  | MINUS NUM { K.NUM (-$2) }
  | NUM { K.NUM ($1) }
  | TRUE { K.TRUE }
  | FALSE { K.FALSE }
  | ID { K.VAR ($1) }
  | expr PLUS expr { K.ADD ($1, $3) }
  | expr MINUS expr  {K.SUB ($1,$3) }
  | expr STAR expr { K.MUL ($1,$3) }
  | expr SLASH expr { K.DIV ($1,$3) }
  | ID LP separated_list(COMMA, expr) RP { K.CALLV ($1,$3) }
	| LC fields RC { K.RECORD $2 }
  | expr PERIOD ID { K.FIELD ($1,$3) }
  | NOT expr { K.NOT ($2) }
  | expr EQUAL expr { K.EQUAL ($1,$3) }

  | expr LB expr { K.LESS ($1, $3) }
  | expr LB exprs RB {
      match $1 with
      | K.VAR x -> K.CALLR (x, $3)
      | _ -> raise (ParsingError "callee of CALLR must be var")
    }

  | IF expr THEN expr ELSE expr { K.IF ($2, $4, $6) }
  | WHILE expr DO expr { K.WHILE ($2, $4) }
  | expr SEMICOLON expr { K.SEQ ($1,$3) }
  | LET ID COLONEQ expr IN expr { K.LETV ($2, $4, $6) }
  | expr COLONEQ expr {
      match $1 with
      | K.VAR x -> K.ASSIGN (x, $3)
      | K.FIELD (e, f) -> K.ASSIGNF (e, f, $3)
      | _ -> raise (ParsingError "lhs of COLONEQ must be either VAR or FIELD")
    }
  | READ ID { K.READ ($2) }
  | WRITE expr { K.WRITE ($2) }
  | LET PROC ID LP vars RP EQUAL expr IN expr { K.LETF ($3, $5, $8, $10) }

  ;
vars:
    separated_list(COMMA, ID) { $1 }
	;
field:
    ID COLONEQ expr { ($1, $3) }
  ;
fields:
    separated_list(COMMA, field) { $1 }
  ;
/* This seemingly unnecessary complication helps resolve most of the conflicts in the grammar. */
%inline exprs:
  | { [] }
  | expr { [$1] }
  | expr COMMA separated_nonempty_list(COMMA, expr) { $1::$3 }
  ;
%%
