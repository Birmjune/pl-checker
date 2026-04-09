(* Exercise 1. calculate *)
open Ex1
open Testlib

(* Left-Riemann with dx=0.1, matching the spec exactly. *)
let riemann_spec a b f =
  let dx = 0.1 in
  if a <= b then begin
    let n = ref 0 in
    let acc = ref 0.0 in
    while a +. dx *. float_of_int !n < b do
      acc := !acc +. dx *. f (a +. dx *. float_of_int !n);
      incr n
    done;
    !acc
  end else begin
    let n = ref 0 in
    let acc = ref 0.0 in
    while b +. dx *. float_of_int !n < a do
      acc := !acc +. dx *. f (b +. dx *. float_of_int !n);
      incr n
    done;
    !acc
  end

module TestEx1 : TestEx =
struct
  type testcase =
    | FLOAT of string * float * exp
    | UNBOUND of string * exp

  let testcases =
    [
      FLOAT ("p1/test01_I_positive", 42.0, I 42);
      FLOAT ("p1/test02_R_pi", 3.14, R 3.14);
      FLOAT ("p1/test03_I_negative", -5.0, I (-5));

      FLOAT ("p1/test04_A_int", 7.0, A (I 3, I 4));
      FLOAT ("p1/test05_S_int", 5.0, S (I 8, I 3));
      FLOAT ("p1/test06_M_int", 12.0, M (I 3, I 4));
      FLOAT ("p1/test07_D_real", 2.5, D (R 5.0, R 2.0));
      FLOAT ("p1/test08_D_int", 3.0, D (I 9, I 3));
      FLOAT ("p1/test09_nested_AM", 14.0, A (M (I 2, I 3), M (I 2, I 4)));
      FLOAT ("p1/test10_nested_SM", 21.0, S (M (I 5, I 5), A (I 2, I 2)));

      UNBOUND ("p1/test11_X_toplevel_unbound", X);
      UNBOUND ("p1/test12_X_in_A_unbound", A (X, I 1));
      UNBOUND ("p1/test13_X_in_M_unbound", M (I 2, X));

      FLOAT ("p1/test14_sigma_hw2_example", 375.0,
        C (I 1, I 10, S (M (X, X), I 1)));
      FLOAT ("p1/test15_sigma_x", 55.0,
        C (I 1, I 10, X));
      FLOAT ("p1/test16_sigma_single", 5.0,
        C (I 5, I 5, X));
      FLOAT ("p1/test17_sigma_empty", 0.0,
        C (I 10, I 1, X));
      FLOAT ("p1/test18_sigma_const", 10.0,
        C (I 1, I 5, I 2));
      FLOAT ("p1/test19_sigma_square", 30.0,
        C (I 0, I 4, M (X, X)));
      FLOAT ("p1/test20_sigma_arith", 36.0,
        C (I 1, I 3, A (X, I 10)));

      FLOAT ("p1/test21_integral_hw2_example",
        riemann_spec 1.0 10.0 (fun x -> x *. x -. 1.0),
        L (R 1.0, R 10.0, S (M (X, X), I 1)));
      FLOAT ("p1/test22_integral_const",
        riemann_spec 0.0 1.0 (fun _ -> 1.0),
        L (R 0.0, R 1.0, I 1));
      FLOAT ("p1/test23_integral_linear",
        riemann_spec 0.0 2.0 (fun x -> x),
        L (R 0.0, R 2.0, X));
      FLOAT ("p1/test24_integral_reversed", -1.0,
        L (R 1.0, R 0.0, I 1));
      FLOAT ("p1/test25_integral_zero_range", 0.0,
        L (R 1.0, R 1.0, X));
      FLOAT ("p1/test26_integral_x_bound",
        riemann_spec 0.0 3.0 (fun x -> x *. x),
        L (R 0.0, R 3.0, M (X, X)));

      (* nested C: sum_{x=1}^{2} sum_{y=x}^{2x} y^2
         x=1 -> 1^2 + 2^2 = 5
         x=2 -> 2^2 + 3^2 + 4^2 = 29
         total = 34 *)
      FLOAT ("p1/test27_nested_sigma", 34.0,
        C (I 1, I 2, C (X, M (I 2, X), M (X, X))));

      (* nested C: sum_{x=1}^{3} sum_{y=1}^{x} y
         x=1 -> 1
         x=2 -> 1+2 = 3
         x=3 -> 1+2+3 = 6
         total = 10 *)
      FLOAT ("p1/test28_nested_sigma_triangle", 10.0,
        C (I 1, I 3, C (I 1, X, X)));

      (* nested C: body A(X,X) uses the inner variable twice
         so this is sum_{x=1}^{3} sum_{y=1}^{2} (y+y)
         each outer iteration: (1+1) + (2+2) = 6
         total = 18 *)
      FLOAT ("p1/test29_nested_sigma_add", 18.0,
        C (I 1, I 3, C (I 1, I 2, A (X, X))));

      (* nested L: integral_{0}^{1} integral_{0}^{1} y dy dx
         inner = 0.0 + 0.1 + ... + 0.9 times 0.1 = 0.45
         outer integrates the constant 0.45 over length 1 -> 0.45 *)
      FLOAT ("p1/test30_nested_integral_const_outer", 0.45,
        L (R 0.0, R 1.0, L (R 0.0, R 1.0, X)));

      (* nested L: integral_{0}^{1} integral_{0}^{x} y dy dx
         bounds X uses the outer variable, body X uses the inner variable
         with the spec's left-Riemann sum, the result is 0.12 *)
      FLOAT ("p1/test31_nested_integral_bound_capture", 0.12,
        L (R 0.0, R 1.0, L (R 0.0, X, X)));

      (* C inside L: integral_{0}^{1} (sum_{i=1}^{3} i) dx
         inner sigma = 1+2+3 = 6
         integral of constant 6 over length 1 = 6.0 *)
      FLOAT ("p1/test32_integral_of_sigma", 6.0,
        L (R 0.0, R 1.0, C (I 1, I 3, X)));

      (* L inside C: sum_{i=1}^{2} integral_{0}^{1} x dx
         inner body X = integration variable, so each integral = 0.45
         total = 0.9 *)
      FLOAT ("p1/test33_sigma_of_integral", 0.9,
        C (I 1, I 2, L (R 0.0, R 1.0, X)));

      (* nested C with independent inner variable:
         sum_{x=1}^{2} sum_{y=1}^{2} y^2 = 2 * (1+4) = 10 *)
      FLOAT ("p1/test34_nested_sigma_independent", 10.0,
        C (I 1, I 2, C (I 1, I 2, M (X, X))));
    ]

  let runner tc =
    match tc with
    | FLOAT (_, expected, e) ->
        (try abs_float (calculate e -. expected) < 1e-4
         with _ -> false)
    | UNBOUND (_, e) ->
        (try let _ = calculate e in false with Unbound -> true | _ -> false)

  let string_of_tc tc =
    match tc with
    | FLOAT (name, expected, e) ->
        let actual_s =
          try string_of_float (calculate e)
          with Unbound -> "Unbound"
             | exn -> Printexc.to_string exn
        in
        (name, string_of_float expected, actual_s)
    | UNBOUND (name, e) ->
        let actual_s =
          try let _ = calculate e in "no exn"
          with Unbound -> "Unbound"
             | exn -> Printexc.to_string exn
        in
        (name, "Unbound", actual_s)
end

open TestEx1
let _ = wrapper testcases runner string_of_tc
