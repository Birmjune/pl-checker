(*
 * SNU 4190.310 Programming Languages
 * Main driver of homework "Continuation Passing Style"
 *)

let main () =
  let print_m = ref false in
  let print_cps = ref false in
  let check_val = ref false in
  let src = ref "" in
  let _ =
    Arg.parse
      [
        ("-pp", Arg.Set print_m, "Print xexp program");
        ("-pcps", Arg.Set print_cps, "Print CPS-converted  program");
        ("-checkval", Arg.Set check_val, "compare run vs cps run; exit 0 if equal");
      ]
      (fun x -> src := x)
      "Usage: ./run [<options>] <xexp file>"
  in

  let lexbuf =
    Lexing.from_channel (if !src = "" then stdin else open_in !src)
  in
  let pgm = Parser.program Lexer.start lexbuf in
  let cps_pgm = Cps.xcps pgm in
  let orig_result = Xexp.run pgm in
  let cps_result =
    Xexp.run
      (Xexp.App
         ( cps_pgm,
           Xexp.Pair (Xexp.Fn ("v", Xexp.Var "v"), Xexp.Fn ("v", Xexp.Num 2025))
         ))
  in

  if !check_val then (
    match orig_result, cps_result with
    | Xexp.N n1, Xexp.N n2 -> exit (if n1 = n2 then 0 else 1)
    | _ -> exit 1
  );

  let _ = print_newline () in
  (if !print_m then
     let _ = print_endline "== Input Program ==" in
     let _ = Xexp.print pgm in
     print_newline ());
  (if !print_cps then
     let _ = print_endline "== CPS-converted Program ==" in
     let _ = Xexp.print cps_pgm in
     print_newline ());
  let _ = print_endline "== Running Input Program with xexp Interpreter ==" in
  let _ =
    match orig_result with
    | Xexp.N n -> print_endline (string_of_int n)
    | _ -> failwith "Program is not evaluated to a number"
  in
  let _ =
    print_endline "== Running converted program with xexp Interpreter =="
  in
  let _ =
    match cps_result with
    | Xexp.N n -> print_endline (string_of_int n)
    | _ -> failwith "Program is not evaluated to a number"
  in
  ()

let _ = main ()
