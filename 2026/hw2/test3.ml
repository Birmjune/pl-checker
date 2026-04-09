(* Exercise 3. leftist heap *)
open Ex3
open Testlib

let value : heap -> int = fun (h) ->
  match h with
  | EMPTY -> -987654321
  | NODE (_,v,_,_) -> v

let rec check_structure : heap -> bool = fun (h) ->
  match h with
  | EMPTY -> true
  | NODE (r,v,lh,rh) -> begin
    (check_structure lh) && (check_structure rh) && (rank lh >= rank rh)
  end

module TestEx3: TestEx =
  struct
    type testcase =
      | SEQ of seq list
    and seq =
      | INSERT of int
      | FINDMIN of int
      | FINDMIN_EMPTY
      | DELETEMIN
      | DELETEMIN_EMPTY

    let testcases =
      [ 
        SEQ [INSERT 1;  INSERT 2; FINDMIN 1; DELETEMIN; FINDMIN 2;];
        SEQ [ DELETEMIN_EMPTY; ];
        SEQ [ DELETEMIN_EMPTY; DELETEMIN_EMPTY; DELETEMIN_EMPTY; DELETEMIN_EMPTY; DELETEMIN_EMPTY;];
        SEQ [ FINDMIN_EMPTY; ];
        SEQ [ FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; FINDMIN_EMPTY; ];
        SEQ [INSERT 1;  INSERT 2; FINDMIN 1; DELETEMIN; FINDMIN 2; DELETEMIN; FINDMIN_EMPTY; DELETEMIN_EMPTY; ];
        SEQ [INSERT 1; INSERT 1; INSERT 1; FINDMIN 1; DELETEMIN; FINDMIN 1; DELETEMIN; FINDMIN 1; DELETEMIN; FINDMIN_EMPTY; ];
        SEQ [INSERT 10; INSERT 5; INSERT 20; INSERT 30; INSERT 0; INSERT 0; FINDMIN 0; DELETEMIN; FINDMIN 0; DELETEMIN
            ;FINDMIN 5; DELETEMIN;FINDMIN 10; DELETEMIN; FINDMIN 20; DELETEMIN; FINDMIN 30; DELETEMIN;FINDMIN_EMPTY; DELETEMIN_EMPTY;];
        SEQ [INSERT 1; INSERT 2; INSERT 3; FINDMIN 1; DELETEMIN; FINDMIN 2; DELETEMIN; FINDMIN 3; DELETEMIN;];
        SEQ [INSERT 3; INSERT 2; INSERT 1; FINDMIN 1; DELETEMIN; FINDMIN 2; DELETEMIN; FINDMIN 3; DELETEMIN;];
        SEQ [INSERT 5; INSERT 5; INSERT 5; FINDMIN 5; DELETEMIN; INSERT 4; FINDMIN 4; DELETEMIN; INSERT 1; FINDMIN 1;];
        SEQ [
          (* A 힙 노드 삽입 *)
          INSERT 1; INSERT 10; INSERT 12;
          INSERT 8; INSERT 11; INSERT 16;

          (* B 힙 노드 삽입 *)
          INSERT 3; INSERT 4; INSERT 7;
          INSERT 6; INSERT 20; INSERT 10;
          INSERT 25; INSERT 21; INSERT 15;
          INSERT 27;

          (* 간단한 검증: 현재 힙에서 최솟값은 1일 것이라 가정 *)
          FINDMIN 1;
          DELETEMIN;
          (* 1 삭제 후 최솟값이 3이라고 가정 *)
          FINDMIN 3
        ];
        SEQ [
          (* 왼쪽 힙의 값들 삽입 *)
          INSERT 1; INSERT 10; INSERT 8; INSERT 12; INSERT 11; INSERT 16;
          (* 오른쪽 힙의 값들 삽입 *)
          INSERT 3; INSERT 4; INSERT 7; INSERT 6; INSERT 20; INSERT 10; INSERT 25; INSERT 21; INSERT 15; INSERT 27;
          (* 전체 힙에 대한 최소값 삭제 순서: 
            병합된 힙에서 최소값은 오름차순으로: 
            1, 3, 4, 6, 7, 8, 10, 10, 11, 12, 15, 16, 20, 21, 25, 27 *)
          FINDMIN 1;  DELETEMIN;  (* 1 삭제 *)
          FINDMIN 3;  DELETEMIN;  (* 3 삭제 *)
          FINDMIN 4;  DELETEMIN;  (* 4 삭제 *)
          FINDMIN 6;  DELETEMIN;  (* 6 삭제 *)
          FINDMIN 7;  DELETEMIN;  (* 7 삭제 *)
          FINDMIN 8;  DELETEMIN;  (* 8 삭제 *)
          FINDMIN 10; DELETEMIN;  (* 첫 번째 10 삭제 *)
          FINDMIN 10; DELETEMIN;  (* 두 번째 10 삭제 *)
          FINDMIN 11; DELETEMIN;  (* 11 삭제 *)
          FINDMIN 12; DELETEMIN;  (* 12 삭제 *)
          FINDMIN 15; DELETEMIN;  (* 15 삭제 *)
          FINDMIN 16; DELETEMIN;  (* 16 삭제 *)
          FINDMIN 20; DELETEMIN;  (* 20 삭제 *)
          FINDMIN 21; DELETEMIN;  (* 21 삭제 *)
          FINDMIN 25; DELETEMIN;  (* 25 삭제 *)
          FINDMIN 27; DELETEMIN;  (* 27 삭제 *)
          FINDMIN_EMPTY; DELETEMIN_EMPTY; (* 힙이 비어있음을 확인 *)
        ];
      ]

    let runner tc =
      let rec runner_ : (seq list) * heap -> bool = fun (l,h) ->
        if (check_structure h) == false then false
        else
        match l with
        | [] -> true
        | (head::tc') -> begin
            match head with
            | INSERT x -> runner_ (tc', insert (x,h))
            | FINDMIN x ->
                let y = findMin h in
                if x = y then runner_ (tc', h)
                else false
            | FINDMIN_EMPTY ->
                let _ = try Some (findMin h) with EmptyHeap -> None in
                true
            | DELETEMIN -> runner_ (tc', deleteMin h)
            | DELETEMIN_EMPTY ->
                let _ = try Some (deleteMin h) with EmptyHeap -> None in
                true
        end
      in
      match tc with
      | SEQ l -> (runner_ (l,EMPTY))

    let string_of_tc tc =
      let rec string_of_seqs : (seq list) * heap -> (string*string*string) = fun (seqs,h) ->
        if (check_structure h) == false then
          ("", "", "Invalid leftist heap structure! (right child's rank is greater than left)" )
        else
        match seqs with
        | [] -> ("", "", "")
        | (head::seqs') ->
            match head with
            | INSERT x -> begin
                let (s, ans, out) = string_of_seqs (seqs',insert (x, h)) in
                ("\n  insert " ^ (string_of_int x)  ^ s, ans, out)
            end

            | FINDMIN x -> begin
                let y = findMin h in
                let (s,ans,out) = string_of_seqs (seqs',h) in
                if x = y then
                  ("\n  findMin: Expected " ^ string_of_int x ^ ", Your output " ^ string_of_int y ^ s, ans, out)
                else
                  ("\n  " ^ wrong_symbol ^ "findMin: Expected " ^ string_of_int x ^ ", Your output " ^ string_of_int y ^ s, ans, out)
            end
            
            | FINDMIN_EMPTY -> begin
                let res = try Some (findMin h) with EmptyHeap -> None in
                match res with
                | Some (y) -> ("\n  " ^ wrong_symbol ^ " findMin", "Exception EmptyHeap", string_of_int y)
                | None ->
                    let (s, ans, out) = string_of_seqs (seqs',h)
                    in ("\n  " ^ correct_symbol ^ "findMin = Exception EmptyHeap" ^ s, ans, out)
            end

            | DELETEMIN ->
                let (s, ans, out) = string_of_seqs (seqs',deleteMin (h)) in
                ("\n  deleteMin()" ^ s, ans, out)
            | DELETEMIN_EMPTY ->
                let res = try Some (deleteMin h) with EmptyHeap -> None in
                match res with
                | Some (y) -> ("\n  " ^ wrong_symbol ^ " deleteMin ", "Exception EmptyHeap", "Non-empty heap")
                | None ->
                    let (s, ans, out) = string_of_seqs (seqs',h)
                    in ("\n  " ^ correct_symbol ^ "deleteMin = Exception EmptyHeap" ^ s, ans, out)
      in
      match tc with
      | SEQ seqs -> string_of_seqs (seqs,EMPTY)
  end

open TestEx3
let _ = wrapper testcases runner string_of_tc
