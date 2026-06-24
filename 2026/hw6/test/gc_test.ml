(* AUTO-GENERATED from PL_sample_answers_2026.txt section 6-2. *)
open Libsm5
open Sm5

exception GC_Incorrect of string

(* Check if malloc_with_gc respects GC spec *)
let malloc =
  let mem_limit = 128 in
  fun ((_, m, _, _, _) as smeck : smeck) ->
    let old_len = List.length m in
    let new_l, new_m = Gc.malloc_with_gc smeck in
    let new_len = List.length new_m in
    if old_len < mem_limit && new_len < old_len then (
      raise (GC_Incorrect "collected when free memory available")
    )
    else if new_len > mem_limit then (
      raise (GC_Incorrect "memory limit exceeded")
    )
    else
      (new_l, new_m)

let run cmd =
  try
    run ~debug:false ~malloc cmd
  with
  | GC_Incorrect msg -> print_endline ("GC_Incorrect: " ^ msg)

(* concat command n times *)
let append (n: int) (f: int -> command) (cmd: command) : command =
  let rec iter i =
    if i = n then []
    else (f i) @ iter (i + 1) in cmd @ (iter 0)

(* ---- generated test harness ---- *)
exception Timeout

let with_timeout secs f =
  let old = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let reset () = ignore (Unix.alarm 0); Sys.set_signal Sys.sigalrm old in
  ignore (Unix.alarm secs);
  match f () with x -> reset (); x | exception e -> reset (); raise e

let run_capture (type a) (f : unit -> a) : (a, exn) result * string =
  flush stdout;
  let tmp = Filename.temp_file "gccheck" ".out" in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let saved = Unix.dup Unix.stdout in
  Unix.dup2 fd Unix.stdout;
  Unix.close fd;
  let r = (try Ok (with_timeout 10 f) with e -> Error e) in
  flush stdout;
  Unix.dup2 saved Unix.stdout;
  Unix.close saved;
  let ic = open_in_bin tmp in
  let s = In_channel.input_all ic in
  close_in ic;
  (try Sys.remove tmp with _ -> ());
  (r, s)

let norm s =
  let rec rtrim = function "" :: tl -> rtrim tl | l -> l in
  String.concat "\n"
    (List.rev (rtrim (List.rev (List.map String.trim (String.split_on_char '\n' s)))))

let _case1 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 1. Simple malloc & use : trigger gc and fail *)
let cmds1 =
  let cmds = append 129 (fun i ->
    let value = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND value;
        PUSH (Val (Z i));
        PUSH (Id value);
        STORE;
      ])
    []
  in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
    let cmds = append 128 (fun i ->
        let value = Printf.sprintf "x%d" i in [
            PUSH (Id value);
            LOAD;
            POP;
         ]) cmds in

    cmds
in
run cmds1
  ) in
  (match r with
   | Error GC_Failure -> (true, "")
   | Ok () -> (false, Printf.sprintf "expected GC_Failure, but completed; output=%S" (norm out))
   | Error e -> (false, "expected GC_Failure, got " ^ Printexc.to_string e))

let _case2 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 2. Simple malloc & use : trigger gc and success *)
let cmds2 =
    (* To be collected *)
    let cmds = [
        PUSH (Val (Z 1));
        MALLOC;
        STORE;
    ] in

    let cmds = append 127 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z 1));
            PUSH (Id v);
            STORE;
        ]) cmds in

    (* Trigger GC *)
    let cmds = cmds @ [
        MALLOC;
        BIND "x_new";
        PUSH (Val (Z 50));
        PUSH (Val (Z 10));
        ADD;
        PUSH (Id "x_new");
        STORE;

        PUSH (Id "x_new");
        LOAD;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds = append 127 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            ADD;
         ]) cmds in

    let cmds = cmds @ [PUT] in

    cmds
in
run cmds2
  ) in
  (match r with
   | Ok () -> let ans = norm {gc|187|gc} in (norm out = ans, Printf.sprintf "expected=%S got=%S" ans (norm out))
   | Error e -> (false, "raised " ^ Printexc.to_string e))

let _case3 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 3. GC must be able to track the location chain : gc fail *)
let cmds3 =
  let cmds = [
    MALLOC;
    BIND "start";
    PUSH (Id "start");
    BIND "cur";
  ] in

  let cmds = append 127 (fun _ ->
    [
      MALLOC;
      PUSH (Id "cur");
      STORE;

      PUSH (Id "cur");
      LOAD;

      UNBIND;
      POP;

      BIND "cur";
    ]) cmds in

  let cmds = cmds @ [PUSH (Val (Z 100)); PUSH (Id "cur"); STORE] in

  (* Trigger GC *)
  let cmds = cmds @ [
    MALLOC;
    BIND "foo";
    PUSH (Val (Z 1));
    PUSH (Id "foo");
    STORE
    ]
  in

  let cmds = cmds @ [PUSH (Val (Z 1)); PUSH (Id "start")] in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
  let cmds = append 127 (fun _ ->
    [LOAD;]
    ) cmds
  in

  cmds @ [STORE]
in
run cmds3
  ) in
  (match r with
   | Error GC_Failure -> (true, "")
   | Ok () -> (false, Printf.sprintf "expected GC_Failure, but completed; output=%S" (norm out))
   | Error e -> (false, "expected GC_Failure, got " ^ Printexc.to_string e))

let _case4 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 4. Gc must be able to track the location chain : gc success *)

let cmds4 =
  (* To be collected *)
  let cmds = [
      PUSH (Val (Z 1));
      MALLOC;
      STORE;
  ] in

  let cmds = cmds @ [
    MALLOC;
    BIND "start";
    PUSH (Id "start");
    BIND "cur";
  ] in

  (* 126 times instead of 127 *)
  let cmds = append 126 (fun _ ->
    [
      MALLOC;
      PUSH (Id "cur");
      STORE;

      PUSH (Id "cur");
      LOAD;

      UNBIND;
      POP;

      BIND "cur";
    ]) cmds in

  let cmds = cmds @ [PUSH (Val (Z 99)); PUSH (Id "cur"); STORE] in

  (* Trigger GC *)
  let cmds = cmds @ [
    MALLOC;
    BIND "foo";
    PUSH (Val (Z 1));
    PUSH (Id "foo");
    STORE
    ]
  in

  let cmds = cmds @ [PUSH (Id "start")] in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
  let cmds = append 126 (fun _ ->
    [LOAD;]
    ) cmds
  in

  cmds @ [LOAD; PUT]
in
run cmds4
  ) in
  (match r with
   | Ok () -> let ans = norm {gc|99|gc} in (norm out = ans, Printf.sprintf "expected=%S got=%S" ans (norm out))
   | Error e -> (false, "raised " ^ Printexc.to_string e))

let _case5 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 5. Alternatedly : gc success *)
let cmds5 =
    (* Trigger GC *)
    let cmds =
    append 128 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            (* To be collected *)
            PUSH (Val (Z 1));
            MALLOC;
            STORE;

            (* Not to be collected *)
            MALLOC;
            BIND v;
            PUSH (Val (Z 10));
            PUSH (Id v);
            STORE
            ])
    [] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds =
    append 128
      (fun i ->
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            ADD;
            ]
      ) (cmds @ [PUSH (Val (Z 0))])
    in

    let cmds = cmds @ [PUT] in

    cmds
in
run cmds5
  ) in
  (match r with
   | Ok () -> let ans = norm {gc|1280|gc} in (norm out = ans, Printf.sprintf "expected=%S got=%S" ans (norm out))
   | Error e -> (false, "raised " ^ Printexc.to_string e))

let _case6 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 6. Alternatedly : gc fail *)
let cmds6 =
  (* Trigger GC *)
  let cmds =
  append 128 (fun i ->
    let v = Printf.sprintf "x%d" i in [
      (* Not to be collected *)
      MALLOC;
      BIND v;
      PUSH (Val (Z 1));
      PUSH (Id v);
      STORE;

      (* To be collected *)
      PUSH (Val (Z 1));
      MALLOC;
      STORE
      ])
  [] in

  (* Check if allocated memory location's values are not affected by GC() *)
  let cmds =
  append 128
    (fun i ->
      let v = Printf.sprintf "x%d" i in [
        PUSH (Id v);
        LOAD;
        ADD;
        ]
    ) (cmds @ [PUSH (Val (Z 0))])
  in

  let cmds = cmds @ [PUT] in

  cmds
in
run cmds6
  ) in
  (match r with
   | Error GC_Failure -> (true, "")
   | Ok () -> (false, Printf.sprintf "expected GC_Failure, but completed; output=%S" (norm out))
   | Error e -> (false, "expected GC_Failure, got " ^ Printexc.to_string e))

let _case7 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 7. Gc must be able to track record : gc fail *)
let cmds7 =
  let cmds = append 124 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND v;
        PUSH (Val (Z i));
        PUSH (Id v);
        STORE;
      ])
  [] in

  let cmds = cmds @ [
    MALLOC;
    BIND "x";
    PUSH (Val (Z 100));
    PUSH (Id "x");
    STORE;

    MALLOC;
    BIND "loc_field";
    PUSH (Id "x");
    PUSH (Id "loc_field");
    STORE;
    UNBIND;

    MALLOC;
    BIND "z_field";
    PUSH (Val (Z 200));
    PUSH (Id "z_field");
    STORE;

    UNBIND;
    BOX 2;

    MALLOC;
    BIND "box";

    PUSH (Id "box");
    STORE;

    (* Trigger GC *)
    PUSH (Val (Z 1));
    MALLOC;
    STORE;
  ] in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
  let cmds = append 124 (fun i ->
      let v = Printf.sprintf "x%d" i in [
          PUSH (Id v);
          LOAD;
          POP;
       ]) cmds
  in

  let cmds = cmds @ [
    PUSH (Id "box");
    LOAD;
    UNBOX "z_field";
    LOAD;
    PUT;
    ]
  in
  let cmds = cmds @ [
    PUSH (Id "box");
    LOAD;
    UNBOX "loc_field";
    LOAD;
    LOAD;
    PUT;
    ]
  in
  cmds
in
run cmds7
  ) in
  (match r with
   | Error GC_Failure -> (true, "")
   | Ok () -> (false, Printf.sprintf "expected GC_Failure, but completed; output=%S" (norm out))
   | Error e -> (false, "expected GC_Failure, got " ^ Printexc.to_string e))

let _case8 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 8. Gc must be able to track record : gc success *)
let cmds8 =
  let cmds = append 123 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND v;
        PUSH (Val (Z i));
        PUSH (Id v);
        STORE;
      ])
  [] in

  let cmds = cmds @ [
    MALLOC;
    BIND "x";
    PUSH (Val (Z 100));
    PUSH (Id "x");
    STORE;

    MALLOC;
    BIND "loc_field";
    PUSH (Id "x");
    PUSH (Id "loc_field");
    STORE;
    UNBIND;

    MALLOC;
    BIND "z_field";
    PUSH (Val (Z 200));
    PUSH (Id "z_field");
    STORE;

    UNBIND;
    BOX 2;

    MALLOC;
    BIND "box";

    PUSH (Id "box");
    STORE;

    (* Trigger GC *)
    PUSH (Val (Z 1));
    MALLOC;
    STORE;
  ] in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
  let cmds = append 123 (fun i ->
      let v = Printf.sprintf "x%d" i in [
          PUSH (Id v);
          LOAD;
          POP;
       ]) cmds
  in

  let cmds = cmds @ [
    PUSH (Id "box");
    LOAD;
    UNBOX "loc_field";
    LOAD;
    LOAD;
    PUT;
    ]
  in

  let cmds = cmds @ [
    PUSH (Id "box");
    LOAD;
    UNBOX "z_field";
    LOAD;
    PUT;
    ]
  in

  cmds
in
run cmds8
  ) in
  (match r with
   | Ok () -> let ans = norm {gc|100
200|gc} in (norm out = ans, Printf.sprintf "expected=%S got=%S" ans (norm out))
   | Error e -> (false, "raised " ^ Printexc.to_string e))

let _case9 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 9. Location allocated in function can be collected in 2nd call : gc success *)
let cmds9 =
    let cmds = [
        PUSH (Fn ("x", [
            (* Trigger GC / At the same time, to be collected in the second call *)
            MALLOC;
            BIND "local";
            PUSH (Val (Z 1));
            PUSH (Id "local");
            STORE;

            (* Access argument location, ensuring it must not have been collected *)
            PUSH (Id "x");
            LOAD;
            POP;
        ]));

        BIND "f";
    ] in

    let cmds = append 126 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z 2));
            PUSH (Id v);
            STORE;
        ]) cmds in

    let cmds = cmds @ [
        MALLOC;
        BIND "arg";

        (* First Call *)
        PUSH (Id "f");
        PUSH (Val (Z 1));
        PUSH (Id "arg");
        CALL;

        (* Second Call *)
        PUSH (Id "f");
        PUSH (Val (Z 2));
        PUSH (Id "arg");
        CALL;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds =
      append 126
        (fun i ->
          let v = Printf.sprintf "x%d" i in
            [PUSH (Id v);
            LOAD;
            ADD]
        ) (cmds @ [PUSH (Val (Z 0));]) in

    let cmds = cmds @ [PUT] in
    cmds
in
run cmds9
  ) in
  (match r with
   | Ok () -> let ans = norm {gc|252|gc} in (norm out = ans, Printf.sprintf "expected=%S got=%S" ans (norm out))
   | Error e -> (false, "raised " ^ Printexc.to_string e))

let _case10 () : bool * string =
  let (r, out) = run_capture (fun () ->
(* 10. Location allocated in function can be collected in 2nd call : gc fail *)
let cmds10 =
    let cmds = [
        PUSH (Fn ("x", [
            (* Trigger GC / At the same time, to be collected in the second call *)
            MALLOC;
            BIND "local";
            PUSH (Val (Z 1));
            PUSH (Id "local");
            STORE;

            (* Access argument location, ensuring it must not have been collected *)
            PUSH (Id "x");
            LOAD;
            POP;
        ]));

        BIND "f";
    ] in

    let cmds = append 126 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z 2));
            PUSH (Id v);
            STORE;
        ]) cmds in

    let cmds = cmds @ [
        MALLOC;
        BIND "arg";

        (* First Call *)
        PUSH (Id "f");
        PUSH (Val (Z 1));
        PUSH (Id "arg");
        CALL;

        (* Allocate and bind new loc *)
        MALLOC;
        BIND "tmp";
        PUSH (Val (Z 3));
        PUSH (Id "tmp");
        STORE;

        (* Second Call *)
        PUSH (Id "f");
        PUSH (Val (Z 2));
        PUSH (Id "arg");
        CALL;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds =
      append 126
        (fun i ->
          let v = Printf.sprintf "x%d" i in
            [PUSH (Id v);
            LOAD;
            ADD]
        ) (cmds @ [PUSH (Val (Z 0));]) in

    let cmds = cmds @ [
      PUSH (Id "tmp");
      LOAD;
      ADD;
      PUT
      ]
    in
    cmds
in
run cmds10
  ) in
  (match r with
   | Error GC_Failure -> (true, "")
   | Ok () -> (false, Printf.sprintf "expected GC_Failure, but completed; output=%S" (norm out))
   | Error e -> (false, "expected GC_Failure, got " ^ Printexc.to_string e))

let cases = [
  ("1. Simple malloc & use : trigger gc and fail", _case1);
  ("2. Simple malloc & use : trigger gc and success", _case2);
  ("3. GC must be able to track the location chain : gc fail", _case3);
  ("4. Gc must be able to track the location chain : gc success", _case4);
  ("5. Alternatedly : gc success", _case5);
  ("6. Alternatedly : gc fail", _case6);
  ("7. Gc must be able to track record : gc fail", _case7);
  ("8. Gc must be able to track record : gc success", _case8);
  ("9. Location allocated in function can be collected in 2nd call : gc success", _case9);
  ("10. Location allocated in function can be collected in 2nd call : gc fail", _case10);
]

let () =
  let total = List.length cases in
  let results =
    List.map
      (fun (n, f) -> let (ok, why) = (try f () with e -> (false, Printexc.to_string e)) in (n, ok, why))
      cases
  in
  let passed = List.length (List.filter (fun (_, ok, _) -> ok) results) in
  List.iteri
    (fun i (n, ok, why) ->
      if not ok then (
        Printf.printf "  \027[31m\xe2\x9c\x97\027[0m Test %d: %s\n" (i + 1) n;
        if why <> "" then Printf.printf "    %s\n" why))
    results;
  let sym = if passed = total then "\027[32m\xe2\x9c\x93\027[0m" else "\027[31m\xe2\x9c\x97\027[0m" in
  Printf.printf "%s Passed %d/%d Cases\n" sym passed total

