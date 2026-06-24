(* Exercise 3. getReady *)
(* testcases from PL_sample_answers_2026.txt section 4-3 *)
open Ex3
open Testlib

(* ---- header (4-3) ---- *)
let compare_key (a : key) (b : key) =
  let rec f (a : t) (b : t) =
    match (a, b) with
    | (Bar, Bar) -> 0
    | (Bar, _) -> -1
    | (_, Bar) -> 1
    | (Node (a1, a2), Node (b1, b2)) ->
        let c = f a1 b1 in
        if c <> 0 then c else f a2 b2
  in
  match (a, b) with
  | Silver a, Silver b -> f a b
  | Silver _, _ -> -1
  | _, Silver _ -> 1
  | Gold a, Gold b -> f a b

let sortedGetReady m =
  List.sort compare_key (getReady m)

let a : map = End (NameBox "a")
let b : map = End (NameBox "b")
let c : map = End (NameBox "c")
let d : map = End (NameBox "d")
let e : map = End (NameBox "e")
let f : map = End (NameBox "f")
let ga k : map = Guide ("a", k)
let gb k : map = Guide ("b", k)
let gc k : map = Guide ("c", k)
let gd k : map = Guide ("d", k)
let ge k : map = Guide ("e", k)
let gf k : map = Guide ("f", k)
let br k1 k2 : map = Branch (k1, k2)
let star : map = End StarBox

(* ---- pretty-print for failure output ---- *)
let rec string_of_t = function
  | Bar -> "Bar"
  | Node (l, r) -> "Node (" ^ string_of_t l ^ ", " ^ string_of_t r ^ ")"
let string_of_key = function
  | Silver t -> "Silver (" ^ string_of_t t ^ ")"
  | Gold t -> "Gold (" ^ string_of_t t ^ ")"
let string_of_key_list ks = "[" ^ String.concat "; " (List.map string_of_key ks) ^ "]"

type expectation = V of key list | RaisesImpossible

let testcases : (string * (unit -> key list) * expectation) list = [
  ("1 : base case", (fun () ->
sortedGetReady star
  ), V ([Silver Bar]));
  ("2 : base case", (fun () ->
sortedGetReady (ga a)
  ), V ([Silver Bar]));
  ("3 : base case", (fun () ->
sortedGetReady ((gb (ga (br a b))))
  ), V ([Silver Bar; Silver (Node (Bar, Bar))]));
  ("4 : base case", (fun () ->
sortedGetReady (br (ga a) star)
  ), V ([Silver Bar]));
  ("5", (fun () ->
getReady (br star star)
  ), RaisesImpossible);
  ("6", (fun () ->
sortedGetReady (br (ga a) (gb b))
  ), V ([Silver Bar; Silver (Node (Bar, Bar))]));
  ("7", (fun () ->
getReady (br (gb (br b b)) star)
  ), RaisesImpossible);
  ("8", (fun () ->
sortedGetReady
(ga (gb (gc (gd (ge (br (br (br (br a b) c) (br d e)) star))))))
  ), V ([Silver Bar; Silver (Node (Bar, Bar)); Silver (Node (Bar, Node (Bar, Node (Bar, Node (Bar, Bar)))))]));
  ("9", (fun () ->
getReady (gb (br (gc (br (br (ga (br a b)) (br b a)) c)) star))
  ), RaisesImpossible);
  ("10", (fun () ->
sortedGetReady
(ga (gb (gc (gd (gf (br (br (br (br a b) c) (br (br d c) f)) (br b star)))))))
  ), V ([Silver Bar; Silver (Node (Bar, Bar)); Silver (Node (Bar, Node (Bar, Bar))); Silver (Node (Node (Bar, Bar), Node (Bar, Node (Bar, Node (Bar, Bar)))))]));
  ("11", (fun () ->
sortedGetReady (br (ga (br a a)) (gb (br b b)))
  ), V ([Gold Bar]));
  ("12", (fun () ->
sortedGetReady (br (ga (br a a)) (gb (gc (br (br b c) b))))
  ), V ([Gold Bar; Gold (Node (Bar, Node (Bar, Bar)))]));
]

let runner (_, run, expect) =
  match expect with
  | V v -> (try run () = v with _ -> false)
  | RaisesImpossible -> (try ignore (run ()); false with IMPOSSIBLE -> true | _ -> false)

let string_of_tc (name, run, expect) =
  let ans = match expect with V v -> string_of_key_list v | RaisesImpossible -> "IMPOSSIBLE" in
  let out =
    try string_of_key_list (run ())
    with IMPOSSIBLE -> "IMPOSSIBLE" | e -> Printexc.to_string e
  in
  (name, ans, out)

let _ = wrapper testcases runner string_of_tc
