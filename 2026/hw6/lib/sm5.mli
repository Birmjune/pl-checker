(*
 * SNU 4190.310 Programming Languages 2026 Spring
 * Skeleton for SM5 & SM5 Limited
 *
 * Refer to the document http://ropas.snu.ac.kr/~kwang/4190.310/26/hw6.pdf for
   SM5's syntax, domain and semantic rules
 *)

type loc = int * int
type record = (string * loc) list
type value = Z of int | B of bool | L of loc | Unit | R of record

type cmd =
  | PUSH of obj
  | POP
  | STORE
  | LOAD
  | JTR of command * command
  | MALLOC
  | BOX of int
  | UNBOX of string
  | BIND of string
  | UNBIND
  | GET
  | PUT
  | CALL
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | LESS
  | NOT

and obj = Val of value | Id of string | Fn of string * command
and command = cmd list

type proc = string * command * environment
and evalue = Loc of loc | Proc of proc
and environment = (string * evalue) list

type svalue = V of value | P of proc | M of (string * evalue)
type stack = svalue list
type memory = (loc * value) list
type continuation = (command * environment) list
type smeck = stack * memory * environment * command * continuation

exception GC_Failure
exception Error of string

val default_malloc : smeck -> loc * memory
val loc_to_str : loc -> string
val val_to_str : value -> string
val cmd_to_str : string -> cmd -> string
val obj_to_str : string -> obj -> string
val command_to_str : string -> command -> string
val proc_to_str : string -> proc -> string
val evalue_to_str : string -> evalue -> string
val env_to_str : string -> environment -> string
val sval_to_str : svalue -> string
val stack_to_str : stack -> string
val mem_to_str : memory -> string
val cont_to_str : continuation -> string
val lookup_env : string -> environment -> evalue
val lookup_record : string -> record -> loc
val load : loc -> memory -> value
val store : loc -> value -> memory -> memory
val box_stack : stack -> int -> record -> stack
val is_equal : value -> value -> bool
val step : smeck -> smeck
val run_helper : smeck -> unit
val run : debug:bool -> malloc:(smeck -> loc * memory) -> command -> unit
