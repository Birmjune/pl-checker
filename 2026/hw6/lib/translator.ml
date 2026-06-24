(*
 * SNU 4190.310 Programming Languages 2026 Spring
 * Homework "SM5"
 *)

(* TODO : complete this function *)
let rec trans : K.program -> Sm5.command = function
  (* basic push *)
  | K.NUM i -> [ Sm5.PUSH (Sm5.Val (Sm5.Z i)) ]
  | K.TRUE -> [ Sm5.PUSH (Sm5.Val (Sm5.B true)) ]
  | K.FALSE -> [ Sm5.PUSH (Sm5.Val (Sm5.B false)) ]

  (* sigma(x)를 찾고 (loc) 그 loc에 해당하는 값을 memory에 load *)
  | K.VAR x -> [ Sm5.PUSH (Sm5.Id x); Sm5.LOAD ]

  (* basic arithmetic 연산들 *)
  (* E는 그대로, 하나 trans 하면 M은 변할수도 있음 *)
  | K.ADD (e1, e2) -> bin_op Sm5.ADD e1 e2
  | K.SUB (e1, e2) -> bin_op Sm5.SUB e1 e2
  | K.MUL (e1, e2) -> bin_op Sm5.MUL e1 e2
  | K.DIV (e1, e2) -> bin_op Sm5.DIV e1 e2
  | K.EQUAL (e1, e2) -> bin_op Sm5.EQ e1 e2
  | K.LESS (e1, e2) -> bin_op Sm5.LESS e1 e2
  | K.NOT e -> trans e @ [ Sm5.NOT ]

  (* 명령 이어서 하기 *)
  | K.SEQ (e1, e2) -> trans e1 @ [ Sm5.POP ] @ trans e2 (* stack에 v2만 남겨야하니 v1 pop *)

  (* 조건문 *)
  | K.IF (e, e1, e2) -> trans e @ [ Sm5.JTR (trans e1, trans e2)] 

  (* 반복문 *)
  | K.WHILE (e, e1) -> 
    (* 일종의 재귀함수로 생각 (인자 0개) *)
    let body = 
      K.LETF (
          "#whilefunc", 
          ["#dummy"], 
          K.IF (e, 
                K.SEQ (e1, K.CALLV ("#whilefunc", [K.NUM 0])), (* true면 e1 실행 후 다시 자기호출 *)
                K.FALSE), (* False면 K.FALSE 반환 *)
          K.CALLV ("#whilefunc", [K.NUM 0])
        )
    in
    trans body
  
  (* let x = e1 in e2 *)
  | K.LETV (x, e1, e2) ->
      trans e1
      @ [ Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE ]
      @ trans e2 @ [ Sm5.UNBIND; Sm5.POP ]
  
  (* x := e *)
  | K.ASSIGN (x, e) ->
      trans e
      @ [ Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD ]

  (* Read 후 저장 *)
  | K.READ x ->
      [ Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD ]

  (* put으로 write *)
  | K.WRITE e ->
      trans e 
      @ [ Sm5.MALLOC; Sm5.BIND "#t";
          Sm5.PUSH (Sm5.Id "#t"); Sm5.STORE; (* #t -> 임시주소 -> n = trans e 저장됨 *)
          Sm5.PUSH (Sm5.Id "#t"); Sm5.LOAD; Sm5.PUT; (* n 출력 *)
          Sm5.PUSH (Sm5.Id "#t"); Sm5.LOAD;
          Sm5.UNBIND; Sm5.POP ] (* 임시변수 bind 해제 *)
    
  | K.LETF (f, xlist, e, e1) ->
    (* CallV / CallR에서 함수 본문 실행 시, f -> p 에서 p가 stack 위에 있는 상황이 온다고 가정 *)
    (* 재귀함수 호출을 위함 *)
    (* 다중 변수의 경우, stack에 함수 인자의 loc 혹은 proc이 온다고 가정 (loc/procN :: ... :: loc/proc1 :: S) *)
    let body =
      [ Sm5.BIND f ] (* 자기호출용 f 맞추기 *)                                      
      @ List.concat_map (fun x -> [ Sm5.BIND x ]) (List.rev xlist) (* stack 순서 맞추기 위해 역순으로 *)
      @ trans e (* 함수 실행 *)
      @ List.concat_map (fun _ -> [ Sm5.UNBIND; Sm5.POP ]) xlist (* bind된 arg들 해제 *)
      @ [ Sm5.UNBIND; Sm5.POP ] (* bind된 f 해제 *)                          
    in
    [ Sm5.PUSH (Sm5.Fn ("#dummy", body)); Sm5.BIND f ] (* dummy 인자 #u를 넘김 *)
    @ trans e1 (* e1 실행 *)
    @ [ Sm5.UNBIND; Sm5.POP ]
  
  | K.CALLV (f, explist) -> 
      let tmp_name i = "#tmp" ^ string_of_int i in 
      List.concat (
        List.mapi (
          fun i e -> (
            trans e (* v_i :: S *)
            @ [ Sm5.MALLOC; Sm5.BIND (tmp_name i); (* tmp_i -> l_i bind, stack은 v_i :: S*)
                Sm5.PUSH (Sm5.Id (tmp_name i));
                Sm5.STORE;
                Sm5.PUSH (Sm5.Id (tmp_name i)); (* stack에는 l_i :: S, Mem에 L_i -> v_i *)
                Sm5.UNBIND; Sm5.POP ] (* Env 정리 *)
          )
        ) explist
      )
      @ [ Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f); 
          Sm5.PUSH (Sm5.Val Sm5.Unit); (* dummy 인자 push (실제 인자는 위의 stack에서 전달됨 )*)
          Sm5.MALLOC; Sm5.CALL ] (* 함수 호출 *)

  | K.CALLR (f, explist) -> 
      (* stack으로 각 인자의 주소를 push 한다. *)
      let process_exps e =
        match e with 
        | K.VAR x -> [ Sm5.PUSH (Sm5.Id x) ]
        | K.FIELD (e', y) -> trans e' @ [ Sm5.UNBOX y ]
        | _ -> failwith ("CALLR args should be var or record access")
      in
      List.concat_map process_exps explist
      @ [ Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f); 
          Sm5.PUSH (Sm5.Val Sm5.Unit);
          Sm5.MALLOC; Sm5.CALL ]

  | K.RECORD idexplist ->
      let process_one_record (x, e) =
        trans e 
        @ [ Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.UNBIND ]
      in
      let z = List.length idexplist in 
      List.concat_map process_one_record idexplist @ [ Sm5.BOX z ] 

  | K.FIELD (e, x) -> trans e @ [ Sm5.UNBOX x; Sm5.LOAD ]

  | K.ASSIGNF (e1, x, e2) ->
    trans e1 
    @ [ Sm5.UNBOX x; Sm5.BIND "#t" ] (* e1 계산 후 r(x)를 임시 var에 할당 *)
    @ trans e2
    @ [ Sm5.PUSH (Sm5.Id "#t"); Sm5.STORE; (* e2 계산 후 r(x) -> v를 mem에 store *)
        Sm5.PUSH (Sm5.Id "#t"); Sm5.LOAD; (* v::S 로 stack 형성 *)
        Sm5.UNBIND; Sm5.POP ] (* Env 정리 *)

and bin_op op e1 e2 =
  trans e2
  @ [ Sm5.MALLOC; Sm5.BIND "#t";
      Sm5.PUSH (Sm5.Id "#t"); Sm5.STORE ] (* #t -> malloc된 주소 -> n2 , stack 그대로 *)
  @ trans e1 (* n1 :: S *)
  @ [ Sm5.PUSH (Sm5.Id "#t"); Sm5.LOAD; (* n2 :: n1 :: S*)
      op;
      Sm5.UNBIND; Sm5.POP ]  (* unbind 후 pop , E 원상복귀 *)  

  