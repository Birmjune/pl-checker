(* Exercise 4. shoppingList *)
open Ex4
open Testlib

module TestEx4: TestEx =
  struct
    type testcase =
      | SHOPPINGLIST of require list * int 
      | FAIL of require list

    let testcases: testcase list =
      [
      SHOPPINGLIST ([(A, []);(B, []);(C, []);(D, []);(E, [])], 0);
      SHOPPINGLIST ([(A, [Same B]);(B, [Same C]);(C, [Same D]);(D, [Same E]);(E, [Same A])], 0);
      SHOPPINGLIST ([
      (A, [Items ([1; 2]); Common(Same B, Same C)]);
      (B, [Common(Same C, Items ([2; 3]))]);
      (C, [Items ([1]); Except(Same A, Items([3])) ]);
      (D, []);(E, [])], 5) ;
      SHOPPINGLIST ([
      (A, [Except(Items ([1; 2]), Same B)]);
      (B, [Except(Same A, Same C)]);
      (C, [Common(Items([2]), Same A) ]);
      (D, []);(E, [])], 2);
      SHOPPINGLIST ([
      (A, [Except(Items ([1]), Same B)]);
      (B, [Items([2])]);
      (C, []); (D, []);(E, [])], 2);
      ]
    
    let rec intersection l1 l2 = 
      match l2 with
      | [] -> []
      | a :: b -> 
        (
          if List.mem a l1 
          then a :: intersection l1 b
          else intersection l1 b
        )

    let rec exclude l1 l2 = (* l1 \setminus l2*)
      match l2 with 
      | [] -> l1
      | a :: b -> 
        (
          exclude (List.filter(fun x -> x <> a) l1) b
        )

    
    let rec includes (l1 : (gift list)) (l2 : (gift list)) : bool = (* checks if l1 is in l2*)
      match l1 with
      | [] -> true
      | a :: b ->
        if List.mem a l2 
        then includes b l2 
        else false

    let rec resolveCond (c : (cond)) (output: (id * gift list) list): gift list =
      match c with
      | Items g -> g
      | Same i -> let _, g = List.find(fun (a, b) -> a = i) output in
        g
      | Common (c1, c2) -> intersection (resolveCond c1 output) (resolveCond c2 output)
      | Except (c1, c2) -> exclude (resolveCond c1 output) (resolveCond c2 output)
    
    let rec checkReq (r : (require)) (output : (id * gift list) list): bool = 
      match r with
      | (i, []) -> true
      | (i, a :: b) -> (
        let gift_list = resolveCond a output in
        let _, ans_gift_list = List.find (fun (x, y) -> x = i) output in
        if (includes gift_list ans_gift_list)
        then checkReq (i, b) output
        else false
      )

    let rec checkPigs input output =
      match input with
      | [] -> true
      | a :: b -> 
        if checkReq a output then 
        checkPigs b output
        else false


    let checkAns (input: (require list) ) (output: (id * (gift list)) list) (ans : int) : bool = 
      let rec checkCost (output: (id * (gift list)) list) : int = 
        match output with
        | [] -> 0
        | (x, y) :: b -> 
          List.length y + checkCost b
        in 
      if checkCost output <> ans 
      then false
      else checkPigs input output


    let runner (tc: testcase): bool =
      match tc with
      | SHOPPINGLIST (r, ans) -> (
          let output =
            try Some (shoppingList r)
            with No_minimum -> None
          in 
          match output with
          | Some o -> checkAns r o ans
          | None -> false
      )
      | FAIL r ->
          let output =
            try Some (shoppingList r)
            with No_minimum -> None
          in output = None
    let string_of_gift_list g = 
      string_of_list string_of_int g
    
    let string_of_id id = 
      match id with
      | A -> "A"
      | B -> "B"
      | C -> "C"
      | D -> "D"
      | E -> "E"
    
    let string_of_output out =
      match out with
      | Some res -> string_of_list (fun (a, b) -> string_of_id a ^ string_of_gift_list b) res
      | None -> "exception No_minimum"
    
    let rec string_of_cond cond = 
      match cond with
      | Items g -> string_of_gift_list g
      | Same i -> string_of_id i
      | Common (cond1, cond2) -> (string_of_cond cond1 ^ "&" ^ string_of_cond cond2)
      | Except (cond1, cond2) -> (string_of_cond cond1 ^ "\\" ^ string_of_cond cond2)
    
    let string_of_require r = 
      string_of_list (fun (a, b) -> string_of_id a ^ (string_of_list string_of_cond b) ) r

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | SHOPPINGLIST (r, ans) -> 
        let output = 
          try Some (shoppingList r)
          with No_minimum -> None
        in
          ( Printf.sprintf "\n  shoppingList  %s" (string_of_require r)
          , string_of_int ans
          , string_of_output output
        )
      | FAIL r ->
          let output =
            try Some (shoppingList r)
            with No_minimum -> None
          in
          ( Printf.sprintf "\n  shoppingList  %s" (string_of_require r)
          , "No_minimum"
          , string_of_output output
          )
  end

open TestEx4
let _ = wrapper testcases runner string_of_tc
