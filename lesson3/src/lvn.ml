
(* implementation of local value numbering *)

module TableKey = struct
  let next_call_counter =
    let counter = ref 0 in
    fun () -> counter := !counter + 1; !counter
  
  type t =
    | KConst of Bril.Const.t
    | KBinary of Bril.Op.Binary.t * string * string
    | KUnary of Bril.Op.Unary.t * string
    | KCall of int

  let of_instr (instr : Bril.Instr.t) lookup_canonical_var =
    let open Bril.Instr in
    match instr with
    | Const (_, const) -> KConst const
    | Binary (_, op, arg1, arg2) ->
      let arg1, arg2 = lookup_canonical_var arg1, lookup_canonical_var arg2 in
      (* normalization *)
      KBinary (op, min arg1 arg2, max arg1 arg2)
    | Unary (_, op, arg) ->
      let arg = lookup_canonical_var arg in
      KUnary (op, arg)
    | Call (Some _, _, _) ->
      (* we have to treat all calls as different *)
      KCall (next_call_counter ())
    | _ -> invalid_arg "invalid instruction passed to of_instr"

  let compare = Stdlib.compare
end

let mk_id dest var =
  Bril.Instr.Unary (dest, Bril.Op.Unary.Id, var)

module VarMap = Map.Make (String)

module Acc = struct
  module KeyTable = Map.Make (TableKey)
  module NumberTable = Map.Make (Int)

  type table_row = {
    num : int;
    key : TableKey.t;
    var : string;
  }

  type key_table = table_row KeyTable.t
  type num_table = table_row NumberTable.t
  type var_map = int VarMap.t

  type acc = {
    count : int;
    key_table : table_row KeyTable.t;
    num_table : table_row NumberTable.t;
    var_map : int VarMap.t;
  }

  let empty = {
    count = 0;
    key_table = KeyTable.empty;
    num_table = NumberTable.empty;
    var_map = VarMap.empty;
  }

  let lookup_canonical_var acc var =
    match VarMap.find_opt var acc.var_map with
    | Some num ->
      begin match NumberTable.find_opt num acc.num_table with
        | Some row -> row.var
        | None -> failwith ("tried looking up missing num: " ^ string_of_int num)
      end
    | None ->
      (* this happens when a variable is defined before the basic block *)
      (* prerr_endline ("could not find canonical var for: " ^ var); *)
      var

  let process_instr acc instr var orig_var =
    (* prerr_endline ("proessing var: " ^ var ^ ", " ^ orig_var); *)
    let key = TableKey.of_instr instr (lookup_canonical_var acc) in
    match KeyTable.find_opt key acc.key_table with
    | Some row ->
      {
        acc with
        var_map = VarMap.add orig_var row.num acc.var_map;
      }
    | None ->
      let id_key = TableKey.KUnary (Bril.Op.Unary.Id, orig_var) in
      let row = {
        num = acc.count;
        key;
        var;
      } in {
        count = acc.count + 1;
        key_table =
          (* this makes [id var] work, possibly *)
          KeyTable.add id_key row acc.key_table
          |> KeyTable.add row.key row;
        num_table = NumberTable.add row.num row acc.num_table;
        var_map = VarMap.add orig_var row.num acc.var_map;
      }

  let lookup_key acc instr =
    KeyTable.find_opt (TableKey.of_instr instr (lookup_canonical_var acc)) acc.key_table
    |> Option.map (fun row -> row.var)
end

let collect_total_writes (bblock : Bril.Instr.t list) =
  let open Bril.Instr in
  List.fold_left
    begin fun total_writes -> function
      | Const ((var, _), _)
      | Binary ((var, _), _, _, _)
      | Unary ((var, _), _, _)
      | Call (Some (var, _), _, _) ->
        VarMap.add var
          begin
            match VarMap.find_opt var total_writes with
            | Some total -> total + 1
            | None -> 1
          end
          total_writes
      | _ -> total_writes
    end VarMap.empty bblock

let fresh_var =
  (* produce a fresh variable name *)
  let counter = ref 0 in
  fun () -> counter := !counter + 1;
    "_fresh" ^ string_of_int !counter

let rename_var total_writes var =
  (* rename a variable if necessary, because it will be overwritten in the future *)
  let remaining = VarMap.find var total_writes in
  let total_writes = VarMap.add var (remaining - 1) total_writes in
  (* prerr_endline ("remaining for " ^ var ^ ": " ^ string_of_int remaining); *)
  match remaining with
  | 1 -> total_writes, var
  | 0 -> failwith "hit zero remaining, not possible"
  | _ -> total_writes, fresh_var ()

let perform_bblock (bblock : Bril.Instr.t list) =
  let open Bril.Instr in
  let (_, _, res) = List.fold_left
    begin fun (total_writes, acc, lst) instr ->
      match instr with
      | Label _ | Jmp _ | Ret None | Nop -> total_writes, acc, instr :: lst

      | Const ((orig_var, dest), const) ->
        let total_writes, var = rename_var total_writes orig_var in
        total_writes, Acc.process_instr acc instr var orig_var, (Const ((var, dest), const)) :: lst

      | Binary ((orig_var, typ), op, arg1, arg2) ->
        let total_writes, var = rename_var total_writes orig_var in
        total_writes,
        Acc.process_instr acc instr var orig_var,
        begin
          match Acc.lookup_key acc instr with
          | Some var' -> mk_id (var, typ) var'
          | None -> Binary ((var, typ), op, Acc.lookup_canonical_var acc arg1, Acc.lookup_canonical_var acc arg2)
        end :: lst

      | Unary ((orig_var, typ), op, arg) ->
        let total_writes, var = rename_var total_writes orig_var in
        total_writes,
        Acc.process_instr acc instr var orig_var,
        begin
          match Acc.lookup_key acc instr with
          | Some var' -> mk_id (var, typ) var'
          | None -> Unary ((var, typ), op, Acc.lookup_canonical_var acc arg)
        end :: lst

      | Br (arg, label1, label2) ->
        total_writes, acc, (Br (Acc.lookup_canonical_var acc arg, label1, label2)) :: lst

      | Call (Some (orig_var, typ), fname, args) ->
        let total_writes, var = rename_var total_writes orig_var in
        total_writes,
        Acc.process_instr acc instr var orig_var,
        (Call (Some (var, typ), fname, List.map (Acc.lookup_canonical_var acc) args)) :: lst

      | Call (None, fname, args) ->
        total_writes, acc, (Call (None, fname, List.map (Acc.lookup_canonical_var acc) args)) :: lst

      | Ret (Some arg) ->
        total_writes, acc, (Ret (Some (Acc.lookup_canonical_var acc arg))) :: lst

      | Print (args) ->
        total_writes, acc, (Print (List.map (Acc.lookup_canonical_var acc) args)) :: lst

    end (collect_total_writes bblock, Acc.empty, []) bblock in
  List.rev res

let perform_func (func : Bril.Func.t) =
  {func with blocks = Core.String.Map.map func.blocks ~f:perform_bblock}

let perform_funcs (funcs : Bril.Func.t list) =
  List.map perform_func funcs
