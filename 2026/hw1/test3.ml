(* Exercise 3. vocalize *)
open Ex3
open Testlib

module TestEx3 : TestEx =
struct
  type testcase =
    | VOCALIZE of string * string list list * string

  let testcases =
    [
      VOCALIZE (
        "p3/test1 (pdf example)",
        [["팔"; "백"; "팔"; "십"]; ["천"; "팔"; "백"; "오"; "십"; "칠"]],
        "8801857"
      );

      VOCALIZE (
        "p3/test2 (pdf example)",
        [["이"]; ["이"; "천"; "구"; "백"]],
        "0022900"
      );

      VOCALIZE (
        "p3/test3",
        [["백"]; ["영"]],
        "1000000"
      );

      VOCALIZE (
        "p3/test4",
        [["천"; "이"; "백"; "삼"; "십"; "사"]; ["오"; "천"; "육"; "백"; "칠"; "십"; "팔"]],
        "12345678"
      );

      VOCALIZE (
        "p3/test5",
        [["백"; "이"; "십"]; ["삼"; "백"; "사"; "십"]],
        "1200340"
      );

      VOCALIZE (
        "p3/test6",
        [["영"]; ["영"]],
        "00000000"
      );

      (* 추가 테스트 *)
      VOCALIZE (
        "p3/test7 all nines",
        [["구"; "천"; "구"; "백"; "구"; "십"; "구"]; ["구"; "천"; "구"; "백"; "구"; "십"; "구"]],
        "99999999"
      );

      VOCALIZE (
        "p3/test8 only back group nonzero",
        [["영"]; ["일"]],
        "00000001"
      );

      VOCALIZE (
        "p3/test9 only front group nonzero",
        [["일"]; ["영"]],
        "00010000"
      );

      VOCALIZE (
        "p3/test10 exact thousands",
        [["천"]; ["천"]],
        "10001000"
      );

      VOCALIZE (
        "p3/test11 exact hundreds tens ones",
        [["백"]; ["십"]],
        "01000010"
      );

      VOCALIZE (
        "p3/test12 mixed internal zeros",
        [["천"; "이"]; ["삼"; "백"; "사"]],
        "10020304"
      );

      VOCALIZE (
        "p3/test13 trailing zeros in both groups",
        [["천"; "이"; "백"]; ["삼"; "천"; "사"; "백"]],
        "12003400"
      );

      VOCALIZE (
        "p3/test14 front zero middle nonzero back zero",
        [["십"]; ["영"]],
        "00100000"
      );

      VOCALIZE (
        "p3/test15 one suppression check",
        [["천"; "백"; "십"; "일"]; ["천"; "백"; "십"; "일"]],
        "11111111"
      );
    ]

  let runner tc =
    match tc with
    | VOCALIZE (_, ans, input) -> vocalize input = ans

  let string_of_str_list l =
    "[" ^ String.concat "; " l ^ "]"

  let string_of_str_list_list ll =
    "[" ^ String.concat "; " (List.map string_of_str_list ll) ^ "]"

  let string_of_tc tc =
    match tc with
    | VOCALIZE (name, ans, input) ->
        (
          name ^ " input: " ^ input,
          string_of_str_list_list ans ^ "\n",
          string_of_str_list_list (vocalize input) ^ "\n"
        )
end

open TestEx3
let _ = wrapper testcases runner string_of_tc