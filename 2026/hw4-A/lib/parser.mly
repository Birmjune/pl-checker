/*
 * SNU 4190.310 Programming Languages 2026 Spring
 *  K- Interpreter
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
%type <K.K.exp> program

%%

program:
       expr EOF { $1 }
    ;

expr:
    LP expr RP { $2 }
  | MINUS NUM { K.K.NUM (-$2) }
  | NUM { K.K.NUM ($1) }
  | TRUE { K.K.TRUE }
  | FALSE { K.K.FALSE }
  | ID { K.K.VAR ($1) }
  | expr PLUS expr { K.K.ADD ($1, $3) }
  | expr MINUS expr  {K.K.SUB ($1,$3) }
  | expr STAR expr { K.K.MUL ($1,$3) }
  | expr SLASH expr { K.K.DIV ($1,$3) }
  | ID LP exprs RP { K.K.CALLV ($1,$3) }
	| LC fields RC { K.K.RECORD $2 }
  | expr PERIOD ID { K.K.FIELD ($1,$3) }
  | NOT expr { K.K.NOT ($2) }
  | expr EQUAL expr { K.K.EQUAL ($1,$3) }

  | expr LB expr { K.K.LESS ($1,$3) }
  | expr LB exprs RB {
      match $1 with
      | K.K.VAR x -> K.K.CALLR (x, $3)
      | _ -> raise (ParsingError "callee of CALLR must be VAR")
    }

  | IF expr THEN expr ELSE expr { K.K.IF ($2, $4, $6) }
  | WHILE expr DO expr { K.K.WHILE ($2, $4) }
  | expr SEMICOLON expr { K.K.SEQ ($1,$3) }
  | LET ID COLONEQ expr IN expr { K.K.LETV ($2, $4, $6) }
  | expr COLONEQ expr {
      match $1 with
      | K.K.VAR x -> K.K.ASSIGN (x, $3)
      | K.K.FIELD (e, f) -> K.K.ASSIGNF (e, f, $3)
      | _ -> raise (ParsingError "lhs of COLONEQ must be either VAR or FIELD")
    }
  | READ ID { K.K.READ ($2) }
  | WRITE expr { K.K.WRITE ($2) }
  | LET PROC ID LP vars RP EQUAL expr IN expr { K.K.LETF ($3, $5, $8, $10) }

  ;
exprs:
    separated_list(COMMA, expr) { $1 }
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
%%
