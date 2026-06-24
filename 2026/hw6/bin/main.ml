(*
 * SNU 4190.310 Programming Languages 2026 Spring
 *
 * Skeleton for SM5 & SM5 Limited
 *)

open Libsm5

let () =
  let pk = ref false in
  let psm5 = ref false in
  let k = ref false in
  let src = ref "" in
  let gc_mode = ref false in
  let debug_mode = ref false in
  let filename = Filename.basename Sys.argv.(0) in
  let _ =
    Arg.parse
      [
        ("-pk", Arg.Set pk, "display K- parse tree");
        ("-psm5", Arg.Set psm5, "print translated Machine code");
        ("-k", Arg.Set k, "run using K interpreter");
        ("-gc", Arg.Set gc_mode, "run with garbage collection");
        ("-debug", Arg.Set debug_mode, "prints machine state every step");
      ]
      (fun x -> src := x)
      ("Usage: " ^ filename ^ " [-pk | -psm5 | -k] [-gc] [-debug] [file]")
  in
  let malloc = if !gc_mode then Gc.malloc_with_gc else Sm5.default_malloc in
  let lexbuf =
    Lexing.from_channel (if !src = "" then stdin else open_in !src)
  in
  let pgm = Parser.program Lexer.start lexbuf in

  if !pk then Pp.pp pgm
  else if !psm5 then
    print_endline (Sm5.command_to_str "" (Translator.trans pgm))
  else if !k then ignore (K.run pgm)
  else Sm5.run ~debug:!debug_mode ~malloc (Translator.trans pgm)
