(*
 * SNU 4190.310 Programming Languages 2026 Spring
 *  K- Interpreter
 *)

open K_
open Pp
open K

let main () =
  let print_code = ref false in
  let src = ref "" in
  let spec =
    [ ("-pp", Arg.Set print_code, "Pretty printing of the input program") ]
  in
  let usage = "Usage : run <options> <file> \nOptions : " in
  let _ =
    Arg.parse spec
      (fun x ->
        if Sys.file_exists x then src := x
        else raise (Arg.Bad (x ^ ": File not found")))
      usage
  in

  if !src = "" then Arg.usage spec usage
  else
    let file_channel = open_in !src in
    let lexbuf = Lexing.from_channel file_channel in
    let pgm = Parser.program Lexer.start lexbuf in
      if !print_code then (
        print_endline "== Input Program ==";
        Kminus_PP.pp pgm)
      else
        try
          ignore (K.run (K.emptyMemory, K.emptyEnv, pgm))
        with K.Error s ->
          print_endline ("Runtime Error : " ^ s)

let _ = main ()
