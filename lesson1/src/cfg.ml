
open Ast

type cfg_node = {
  labels : label list;
  instrs : instr list;
  mutable dests : cfg_node list;
}

type acc = cfg_node list * instr list * string list

let cfg_of_func (func : func) =
  let narrow_instr instr : instr =
    match instr with
    | `Label label -> failwith "error: got label in narrow_instr"
    | `EInstr e -> `EInstr e
    | `RInstr r -> `RInstr r in
  
  let form_bblock bblocks bblock labels =
    match bblock with
    | _ :: _ -> {labels = labels; instrs = List.rev bblock; dests = []} :: bblocks
    | [] -> bblocks in

  let bblock_collect ((bblocks, bblock, blabels) : acc) (instr : instr_or_label) : acc =
    match instr with
    | `Label label ->
      form_bblock bblocks bblock blabels, [], label :: blabels
    | `EInstr (IJump _) ->
      form_bblock bblocks (narrow_instr instr :: bblock) blabels, [], []
    | `EInstr (IBranch _) ->
      form_bblock bblocks (narrow_instr instr :: bblock) blabels, [], []
    | `EInstr (IReturn _) ->
      form_bblock bblocks (narrow_instr instr :: bblock) blabels, [], []
    | _ -> bblocks, narrow_instr instr :: bblock, blabels in

  let bblocks, bblock, labels = List.fold_left bblock_collect ([], [], []) func.instrs in
  let bblocks = form_bblock bblocks bblock labels |> List.rev in

  let open Hashtbl.Make (struct
      type t = string
      let equal = (=)
      let hash = Hashtbl.hash
    end) in

  let ht = create 10 in

  List.iter (fun cfg_node ->
      List.iter (fun label ->
          add ht label cfg_node)
        cfg_node.labels)
    bblocks;

  let add_cfg_dest label cfg_node =
    match find_opt ht label with
    | Some cfg -> cfg_node.dests <- cfg :: cfg_node.dests
    | None -> (* failwith ("could not find label: " ^ label) *)
      (* this can happen if there is a label at the very end *)
      () in

  let connect_dests_internal cfg_node next_cfg_node =
    match List.rev cfg_node.instrs |> List.hd with
    | `EInstr (IJump dest) -> add_cfg_dest dest cfg_node
    | `EInstr (IBranch {t; f}) ->
      begin
        add_cfg_dest t cfg_node;
        add_cfg_dest f cfg_node
      end
    | `EInstr (IReturn _) -> ()
    | _ ->
      begin
        match next_cfg_node with
        | Some dest -> cfg_node.dests <- dest :: cfg_node.dests
        | None -> ()
      end in

  let rec connect_dests = function
    | hd :: md :: tl ->
      begin
        connect_dests_internal hd (Some md);
        connect_dests (md :: tl)
      end
    | hd :: [] ->
      connect_dests_internal hd None
    | [] -> () in

  connect_dests bblocks;
  bblocks
