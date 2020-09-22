
(* implementation of "trivial" dead code elimination *)

let perform_bblock (bblock : Bril.Instr.t list) =
  let open Core.String.Set in
  let open Bril.Instr in
  (* we just assume that all vars are used by successor bblocks *)
  let all_vars =
    List.fold_left
      begin fun set -> function
        | Const ((var, _), _)
        | Binary ((var, _), _, _, _)
        | Unary ((var, _), _, _) -> add set var
        | _ -> set
      end empty bblock in
  bblock
  |> List.rev
  |> List.fold_left
    begin fun (set, lst) instr ->
      match instr with
      | Label _ -> failwith "got a label in a basic block"
      | Const ((var, _), _) ->
        if mem set var
        then remove set var, instr :: lst
        else set, lst
      | Binary ((var, _), _, arg1, arg2) ->
        if mem set var
        then add (add (remove set var) arg1) arg2, instr :: lst
        else set, lst
      | Unary ((var, _), _, arg) ->
        if mem set var
        then add (remove set var) arg, instr :: lst
        else set, lst
      | Jmp _ -> set, instr :: lst
      | Br (arg, _, _) -> add set arg, instr :: lst
      | Call (Some (var, _), _, args) -> List.fold_left add (remove set var) args, instr :: lst
      | Call (None, _, args) -> List.fold_left add set args, instr :: lst
      | Ret (Some arg) -> add set arg, instr :: lst
      | Ret None -> set, instr :: lst
      | Print args -> List.fold_left add set args, instr :: lst
      | Nop -> set, instr :: lst
    end (all_vars, [])
  |> snd

let perform_func (func : Bril.Func.t) =
  {func with blocks = Core.String.Map.map func.blocks ~f:perform_bblock}

let perform_funcs (funcs : Bril.Func.t list) =
  List.map perform_func funcs

let rec perform_until_convergence funcs =
  (* for some reason this is failing, saying we're comparing functions?? *)
  let funcs' = perform_funcs funcs in
  if funcs = funcs' then funcs'
  else perform_until_convergence funcs'
