(* Exercise 2. diff *)
open Ex2
open Testlib

(* Hash function for handling minus powers *)
let hash : int * int -> int = fun (a, b) ->
  a * 37 + b * 1000000009

(* Simple integer exponential *)
let rec pow : int * int -> int = fun (a, b) ->
  if b < 0 then hash (a, b)
  else if b = 0 then 1
  else a * (pow (a, b - 1))

(* Simple list sort *)
let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)
and insert elem = function
  | [] -> [elem]
  | x :: l -> if elem < x then elem :: x :: l
              else x :: insert elem l

let rec print_list = function
  | [] -> ()
  | e :: l ->
      print_string (string_of_bool e);
      print_string " ";
      print_list l

(* Remove any duplicates in list. *)
let rec remove_dups lst =
  match lst with
  | [] -> []
  | h :: t -> h :: (remove_dups (List.filter (fun x -> x <> h) t))

(* String representation of polynomial. Might be dirty. *)
let rec string_of_ae : ae -> string = fun f ->
  match f with
  | CONST a -> string_of_int a
  | VAR a -> a
  | POWER (a, b) -> a ^ "^" ^ string_of_int b
  | TIMES l ->
      begin match l with
      | [] -> ""
      | [a] -> string_of_ae a
      | a :: b -> "(" ^ string_of_ae a ^ " " ^ string_of_ae (TIMES b) ^ ")"
      end
  | SUM l ->
      begin match l with
      | [] -> ""
      | [a] -> string_of_ae a
      | a :: b -> "(" ^ string_of_ae a ^ "+" ^ string_of_ae (SUM b) ^ ")"
      end

(*
  Calculate value of ae which only contains CONST.
  Make sure to replace every VAR into CONST before using this.
*)
let rec calc : ae -> int = fun f ->
  match f with
  | CONST a -> a
  | VAR _ -> 0
  | POWER _ -> 0
  | TIMES l ->
      begin match l with
      | [] -> 1
      | [a] -> calc a
      | a :: b -> calc a * calc (TIMES b)
      end
  | SUM l ->
      begin match l with
      | [] -> 1
      | [a] -> calc a
      | a :: b -> calc a + calc (SUM b)
      end

(*
  Replace every specific VAR s into given CONST x.
*)
let rec replace : ae * string * int -> ae = fun (f, s, x) ->
  match f with
  | CONST a -> CONST a
  | VAR a -> if a = s then CONST x else VAR a
  | POWER (a, b) -> if a = s then CONST (pow (x, b)) else POWER (a, b)
  | TIMES l ->
      begin match l with
      | [] -> CONST 1
      | [a] -> replace (a, s, x)
      | a :: b -> TIMES [replace (a, s, x); replace (TIMES b, s, x)]
      end
  | SUM l ->
      begin match l with
      | [] -> CONST 0
      | [a] -> replace (a, s, x)
      | a :: b -> SUM [replace (a, s, x); replace (SUM b, s, x)]
      end

(* Get list of every VAR exists in given ae *)
let rec get_vallist : ae -> string list = fun f ->
  match f with
  | CONST _ -> []
  | VAR a -> [a]
  | POWER (a, _) -> [a]
  | TIMES l ->
      begin match l with
      | [] -> []
      | a :: b -> List.concat [get_vallist a; get_vallist (TIMES b)]
      end
  | SUM l ->
      begin match l with
      | [] -> []
      | a :: b -> List.concat [get_vallist a; get_vallist (SUM b)]
      end

(* Try [-5,5] for every VAR exists in given ae *)
let rec backtrack : ae * ae * string list * int * int -> bool list =
 fun (f, g, l, cur, mx) ->
  if cur > mx then []
  else
    match l with
    | [] ->
        if calc f = calc g then [true] else [false]
    | a :: b ->
        let ff = replace (f, a, cur) in
        let gg = replace (g, a, cur) in
        List.concat [backtrack (f, g, l, cur + 1, mx); backtrack (ff, gg, b, -5, mx)]

(* Check if a list has only true in it *)
let rec only_true : bool list -> bool = fun lst ->
  match lst with
  | [] -> true
  | h :: t -> if h = false then false else only_true t

(* Check if two ae are equal by calculate and compare values on many points *)
let check_equal : ae * ae -> bool = fun (f, g) ->
  let fvars = get_vallist f in
  let gvars = get_vallist g in
  let total_vars = List.concat [fvars; gvars] in
  let unique_vars = remove_dups total_vars in
  let arr = backtrack (f, g, unique_vars, -5, 5) in
  only_true arr

module TestEx2 : TestEx =
struct
  type testcase =
    | DIFF of ae * string * ae
    | DIFF_INVALID of ae * string

  let testcases =
    [
      (* TA testcase *)
      DIFF (
        SUM [TIMES [CONST 5; TIMES [VAR "x"; VAR "x"]]; CONST 1],
        "x",
        TIMES [CONST 10; VAR "x"]
      );

      (* Original testcases *)
      DIFF (VAR "x", "x", CONST 1);
      DIFF (VAR "x", "y", CONST 0);
      DIFF (VAR "x", "z", CONST 0);
      DIFF (VAR "wow", "wow", CONST 1);
      DIFF (POWER ("x", 5), "x", TIMES [CONST 5; POWER ("x", 4)]);
      DIFF (VAR "wow", "fsdafsddfs", CONST 0);
      DIFF (TIMES [CONST 5; VAR "x"], "x", CONST 5);
      DIFF (TIMES [CONST 5; VAR "x"], "y", CONST 0);
      DIFF (TIMES [CONST 5; VAR "x"; VAR "y"], "x", TIMES [CONST 5; VAR "y"]);
      DIFF (TIMES [CONST 5; VAR "x"; VAR "y"], "y", TIMES [CONST 5; VAR "x"]);
      DIFF (TIMES [CONST 5; VAR "x"; VAR "y"], "z", CONST 0);
      DIFF (POWER ("x", 5), "x", TIMES [CONST 5; POWER ("x", 4)]);
      DIFF (POWER ("x", 0), "x", TIMES [CONST 0; POWER ("x", -1)]);
      DIFF (POWER ("x", -1), "x", TIMES [CONST (-1); POWER ("x", -2)]);
      DIFF (SUM [CONST 1; VAR "x"; VAR "y"; VAR "z"], "x", CONST 1);
      DIFF (SUM [CONST 1; VAR "x"; VAR "y"; VAR "z"], "y", CONST 1);
      DIFF (SUM [CONST 1; VAR "x"; VAR "y"; VAR "z"], "z", CONST 1);
      DIFF (SUM [CONST 1; VAR "x"; VAR "y"; VAR "z"], "asdf", CONST 0);
      DIFF (TIMES [CONST 5; CONST 6; VAR "x"], "x", CONST 30);
      DIFF (TIMES [CONST 5; CONST 6; VAR "y"; VAR "x"], "x", TIMES [CONST 30; VAR "y"]);
      DIFF (
        SUM [TIMES [VAR "x"; VAR "y"]; TIMES [VAR "y"; POWER ("x", 6)]],
        "y",
        SUM [TIMES [VAR "x"]; POWER ("x", 6)]
      );
      DIFF (
        SUM [TIMES [VAR "x"; VAR "y"]; TIMES [VAR "y"; POWER ("x", 6)]],
        "x",
        SUM [VAR "y"; TIMES [CONST 6; POWER ("x", 5); VAR "y"]]
      );
      DIFF (
        SUM [TIMES [VAR "x"; VAR "y"]; TIMES [VAR "y"; POWER ("x", 6)]],
        "z",
        CONST 0
      );
      DIFF (
        TIMES [SUM [VAR "x"; VAR "y"]; SUM [VAR "y"; VAR "z"]; SUM [VAR "z"; VAR "x"]],
        "x",
        SUM [
          TIMES [VAR "y"; VAR "y"];
          POWER ("z", 2);
          TIMES [CONST 2; SUM [TIMES [VAR "x"; VAR "y"]; TIMES [VAR "y"; VAR "z"]; TIMES [VAR "z"; VAR "x"]]]
        ]
      );
      DIFF (
        TIMES [CONST 5; SUM [VAR "x"; VAR "y"]; SUM [VAR "x"; VAR "y"]; SUM [VAR "x"; VAR "y"]],
        "y",
        TIMES [CONST 15; SUM [VAR "x"; VAR "y"]; SUM [VAR "x"; VAR "y"]]
      );
      DIFF_INVALID (TIMES [], "x");
      DIFF_INVALID (SUM [], "x");
      DIFF_INVALID (TIMES [CONST 3; SUM []], "x");
      DIFF_INVALID (SUM [TIMES [VAR "x"; VAR "y"]; TIMES []], "x");

      (* Additional tests *)
      DIFF (CONST 1, "x", CONST 0);
      DIFF (VAR "y", "x", CONST 0);
      DIFF (POWER ("xxx", 4), "xxx", TIMES [CONST 4; POWER ("xxx", 3)]);
      DIFF (POWER ("xy", 3), "yx", CONST 0);
      DIFF (
        TIMES [CONST 3; VAR "x"; POWER ("y", 2); VAR "z"],
        "y",
        TIMES [CONST 6; VAR "x"; VAR "y"; VAR "z"]
      );
      DIFF (
        SUM [TIMES [CONST 3; VAR "x"; POWER ("y", 2); VAR "z"]; VAR "y"],
        "x",
        TIMES [CONST 3; POWER ("y", 2); VAR "z"]
      );
      DIFF (
        SUM [TIMES [CONST 2; POWER ("x", 2)]; TIMES [CONST 3; VAR "x"]; CONST 4],
        "x",
        SUM [TIMES [CONST 4; VAR "x"]; CONST 3]
      );
      DIFF (
        SUM [TIMES [CONST 10; VAR "x"]; VAR "y"],
        "xy",
        CONST 0
      );
      DIFF (
        TIMES [VAR "x"; VAR "x"],
        "x",
        TIMES [CONST 2; VAR "x"]
      );
      DIFF (
        SUM [POWER ("x", 2); TIMES [CONST 2; VAR "x"]; CONST 1],
        "x",
        SUM [TIMES [CONST 2; VAR "x"]; CONST 2]
      );
      DIFF (
        SUM [POWER ("x", 2); POWER ("x", 2); CONST 1],
        "x",
        TIMES [CONST 4; VAR "x"]
      );
      DIFF (
        SUM [POWER ("x", 2); POWER ("x", 2); CONST 1],
        "y",
        CONST 0
      );
      DIFF (
        TIMES [POWER ("x", 3); POWER ("y", 2)],
        "x",
        TIMES [CONST 3; POWER ("x", 2); POWER ("y", 2)]
      );
      DIFF (
        SUM [
          TIMES [SUM [VAR "x"; VAR "y"]; TIMES [VAR "x"; VAR "y"]];
          POWER ("x", 2)
        ],
        "x",
        SUM [
          SUM [
            TIMES [CONST 1; TIMES [VAR "x"; VAR "y"]];
            TIMES [SUM [VAR "x"; VAR "y"]; VAR "y"]
          ];
          TIMES [CONST 2; VAR "x"]
        ]
      );
      DIFF (
        TIMES [TIMES [SUM [VAR "x"; VAR "y"]; VAR "x"]; VAR "x"],
        "x",
        SUM [
          TIMES [
            SUM [
              TIMES [CONST 1; VAR "x"];
              TIMES [SUM [VAR "x"; VAR "y"]; CONST 1]
            ];
            VAR "x"
          ];
          TIMES [TIMES [SUM [VAR "x"; VAR "y"]; VAR "x"]; CONST 1]
        ]
      );
      DIFF (
        TIMES [POWER ("x", 2); VAR "y"],
        "x",
        TIMES [CONST 2; VAR "x"; VAR "y"]
      );
      DIFF (
        TIMES [CONST 2; SUM [VAR "x"; VAR "y"]; POWER ("x", 3)],
        "x",
        SUM [
          TIMES [CONST 2; CONST 1; POWER ("x", 3)];
          TIMES [CONST 2; SUM [VAR "x"; VAR "y"]; TIMES [CONST 3; POWER ("x", 2)]]
        ]
      );
      DIFF (
        TIMES [
          SUM [VAR "x"; VAR "y"; VAR "z"];
          POWER ("x", 2);
          SUM [TIMES [CONST 3; VAR "x"]; VAR "z"]
        ],
        "x",
        SUM [
          TIMES [CONST 1; POWER ("x", 2); SUM [TIMES [CONST 3; VAR "x"]; VAR "z"]];
          TIMES [SUM [VAR "x"; VAR "y"; VAR "z"]; TIMES [CONST 2; VAR "x"]; SUM [TIMES [CONST 3; VAR "x"]; VAR "z"]];
          TIMES [SUM [VAR "x"; VAR "y"; VAR "z"]; POWER ("x", 2); CONST 3]
        ]
      );
      DIFF (
        TIMES [
          SUM [VAR "x"; VAR "y"; VAR "z"];
          POWER ("x", 2);
          SUM [TIMES [CONST 3; VAR "x"]; VAR "z"]
        ],
        "y",
        TIMES [POWER ("x", 2); SUM [TIMES [CONST 3; VAR "x"]; VAR "z"]]
      );
    ]

  let runner tc =
    match tc with
    | DIFF (f, var, ans) -> check_equal (diff (f, var), ans)
    | DIFF_INVALID (f, var) ->
        (try check_equal (diff (f, var), CONST 1)
         with InvalidArgument -> true)

  let string_of_tc tc =
    match tc with
    | DIFF (f, var, ans) ->
        ( string_of_ae f ^ " diff by " ^ var,
          string_of_ae ans ^ "\n",
          string_of_ae (diff (f, var)) ^ "\n" )
    | DIFF_INVALID (f, var) ->
        ( string_of_ae f ^ " diff by " ^ var,
          "Expected InvalidArgument Exception\n",
          string_of_ae (diff (f, var)) ^ "\n" )
end

open TestEx2

let _ = wrapper testcases runner string_of_tc