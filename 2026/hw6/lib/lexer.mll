(*
 * SNU 4190.310 Programming Languages 2026 Spring
 * Skeleton for SM5 & SM5 Limited
 *)

{
open Parser

exception LexicalError of string

let comment_depth = ref 0
}

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start = parse
  | blank { start lexbuf }
  | "(*" { comment_depth :=1;
           comment lexbuf;
           start lexbuf }
  | number { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "proc" { PROC }
  | "while" { WHILE }
  | "do" { DO }
  | "read" { READ }
  | "write" { WRITE }
  | id { ID (Lexing.lexeme lexbuf) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "=" { EQUAL }
  | "<" { LB }
  | ">" { RB }
  | ":=" { COLONEQ }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "." { PERIOD }
  | "(" { LP }
  | ")" { RP }
  | "{" { LC }
  | "}" { RC }
  | eof { EOF}
  | _ { raise (LexicalError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
and comment = parse
  | "(*" { comment_depth := !comment_depth+1; comment lexbuf}
  | "*)" { comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
  | eof { raise (LexicalError "Unterminated comment") }
  | _   { comment lexbuf }
