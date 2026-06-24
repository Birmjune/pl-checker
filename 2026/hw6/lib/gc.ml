(*
 * SNU 4190.310 Programming Languages 2026 Spring
 * Homework "SM5 Limited" -- base-aware corrected GC
 *
 * Sound for arbitrary SM5 programs (not only K-- translations):
 *   - follows loc values stored in memory cells (not just records)
 *   - marks at base granularity, so a cell kept via base-expansion
 *     (reachable through loc arithmetic) also has its contents walked
 *)

open Sm5

let malloc_with_gc =
  let mem_limit = 128 in
  let loc_id = ref 0 in
  let reachable_bases : int list ref = ref [] in
  fun ((s, m, e, c, k) : smeck) ->
    if List.length m < mem_limit then (
      loc_id := !loc_id + 1;
      ((!loc_id, 0), m))
    else begin
      reachable_bases := [];
      (* When a base becomes reachable, walk EVERY memory cell of that base
         and follow the pointers (L / R) it holds. Base granularity is sound
         under loc arithmetic, which can reach any offset of a base. *)
      let rec add_loc (b, _) =
        if not (List.mem b !reachable_bases) then begin
          reachable_bases := b :: !reachable_bases;
          List.iter (fun ((cb, _), v) -> if cb = b then mark_value v) m
        end
      and mark_value = function
        | L l -> add_loc l
        | R r -> List.iter (fun (_, rl) -> add_loc rl) r
        | _ -> ()
      and mark_evalue = function Loc l -> add_loc l | Proc p -> mark_proc p
      and mark_proc (_, cc, ee) = mark_cmd cc; mark_env ee
      and mark_svalue = function
        | V v -> mark_value v
        | P p -> mark_proc p
        | M (_, ev) -> mark_evalue ev
      and mark_env ee = List.iter (fun (_, ev) -> mark_evalue ev) ee
      and mark_cont kk = List.iter (fun (cc, ee) -> mark_cmd cc; mark_env ee) kk
      and mark_cmd cc = List.iter mark_one_cmd cc
      and mark_one_cmd = function
        | PUSH (Val v) -> mark_value v
        | PUSH (Fn (_, c')) -> mark_cmd c'
        | JTR (c1, c2) -> mark_cmd c1; mark_cmd c2
        | _ -> ()
      in
      List.iter mark_svalue s;
      mark_env e;
      mark_cont k;
      mark_cmd c;
      let new_m =
        List.filter (fun ((b, _), _) -> List.mem b !reachable_bases) m
      in
      if List.length new_m < mem_limit then (
        loc_id := !loc_id + 1;
        ((!loc_id, 0), new_m))
      else raise GC_Failure
    end
