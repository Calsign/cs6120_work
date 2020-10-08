
open Dom

(** [vars func] is the set of all variable names defined in [func]. *)
let vars func : Core.String.Set.t =
  let open Bril.Func in
  let open Bril.Instr in
  let set =
    Core.String.Map.fold
      func.blocks
      ~init:Core.String.Set.empty
      ~f:begin fun ~key ~data acc ->
        List.fold_left
          begin fun acc -> function
            | Const ((var, _), _)
            | Binary ((var, _), _, _, _)
            | Unary ((var, _), _, _)
            | Call (Some (var, _), _, _)
            | Phi ((var, _), _) ->
              Core.String.Set.add acc var
            | Label _
            | Jmp _
            | Br (_, _, _)
            | Call (None, _, _)
            | Ret _
            | Print _
            | Nop ->
              acc
          end
          acc data
      end in
  List.fold_left
    begin fun acc (param, _) ->
      Core.String.Set.add acc param
    end
    set
    func.args

(** [defs func var] is the set of all blocks that define variable
   [var] in [func]. *)
let defs func var : Core.String.Set.t =
  let open Bril.Func in
  let open Bril.Instr in
  Core.String.Map.fold
    func.blocks
    ~init:Core.String.Set.empty
    ~f:begin fun ~key ~data acc ->
      List.fold_left
        begin fun acc -> function
          | Const ((var', _), _)
          | Binary ((var', _), _, _, _)
          | Unary ((var', _), _, _)
          | Call (Some (var', _), _, _)
          | Phi ((var', _), _)
            when var = var' ->
            Core.String.Set.add acc key
          | _ -> acc
        end
        acc data
    end

let insert_phi_node
    (var : string)
    (typ : Bril.Bril_type.t)
    (block : Bril.Instr.t list)
  : Bril.Instr.t list =

  let open Bril.Instr in

  let rec aux = function
    | (Phi ((var', _), _)) :: tl when var = var' -> true
    | (Phi _) :: tl -> aux tl
    | (Label _) :: tl -> aux tl
    | _ -> false in

  if aux block then block
  (* need to keep the label first *)
  else (List.hd block) :: (Phi ((var, typ), [])) :: (List.tl block)

let rec insert_phi_nodes
    (var : string)
    (defs : Core.String.Set.t)
    (blocks : Bril.Instr.t list Core.String.Map.t)
    (df : Core.String.Set.t Core.String.Map.t)
  : Bril.Instr.t list Core.String.Map.t =

  Core.String.Set.fold
    defs
    ~init:(defs, blocks)
    ~f:begin fun acc def ->
      Core.String.Set.fold
        (Core.String.Map.find_exn df def)
        ~init:acc
        ~f:begin fun (defs', blocks') block ->
          let defs'' = Core.String.Set.add defs' block in
          let blocks'' =
            Core.String.Map.change
              blocks'
              block
              ~f:begin function
                | Some instrs ->
                  (* for now arbitrarily pick a type,
                     we will do type-checking later *)
                  Some (insert_phi_node var Bril.Bril_type.IntType instrs)
                | None -> failwith ("missing block: " ^ block)
              end
          in
          (defs'', blocks'')
        end
    end
  |> snd

let unique_var =
  let counter = ref 0 in
  fun name -> counter := !counter + 1;
    (String.index_opt name '.'
     |> Option.value ~default:(String.length name)
     |> String.sub name 0) ^ "." ^ string_of_int !counter

let rec rename_variables
    (block : string)
    (var_stack : string list Core.String.Map.t)
    (blocks : Bril.Instr.t list Core.String.Map.t)
    (dt : Core.String.Set.t Core.String.Map.t)
    (succs : string list Core.String.Map.t)
  : Bril.Instr.t list Core.String.Map.t * string list Core.String.Map.t =

  (* print_endline "";
   * print_endline ("block: " ^ block);
   * 
   * Printers.string_of_string_set_string_map (Core.String.Map.map var_stack ~f:Core.String.Set.of_list) |> print_endline; *)

  let open Bril.Instr in

  let instrs = Core.String.Map.find_exn blocks block in

  let instrs', var_stack' =
    
    let replace_dest (var, typ) var_stack'' =
      let var' = unique_var var in
      let var_stack''' =
        Core.String.Map.change var_stack'' var
          ~f:begin function
            | Some stack -> Some (var' :: stack)
            | None -> failwith ("missing var in var stack: " ^ var)
          end in
      let var_stack'''' =
        Core.String.Map.change var_stack''' var'
          ~f:begin function
            | Some stack -> Some stack
            | None -> Some [var']
          end in
      ((var', typ), var_stack'''') in

    let replace_arg var_stack'' arg =
      match Core.String.Map.find_exn var_stack'' arg with
      | hd :: _ -> hd
      | [] -> failwith ("empty stack for arg: " ^ arg) in

    List.fold_left
      begin fun (acc, var_stack') -> function
        | Label label -> ((Label label) :: acc, var_stack')

        | Const (dest, const) ->
          let dest', var_stack'' = replace_dest dest var_stack' in
          ((Const (dest', const)) :: acc, var_stack'')

        | Binary (dest, op, arg1, arg2) ->
          let dest', var_stack'' = replace_dest dest var_stack' in
          ((Binary (dest', op, replace_arg var_stack' arg1,
                    replace_arg var_stack' arg2)) :: acc, var_stack'')

        | Unary (dest, op, arg) ->
          let dest', var_stack'' = replace_dest dest var_stack' in
          ((Unary (dest', op, replace_arg var_stack' arg)) :: acc, var_stack'')

        | Jmp label -> ((Jmp label) :: acc, var_stack')

        | Br (arg, l1, l2) ->
          ((Br (replace_arg var_stack' arg, l1, l2)) :: acc, var_stack')

        | Call (Some dest, fname, args) ->
          let dest', var_stack'' = replace_dest dest var_stack' in
          ((Call (Some dest', fname,
                  List.map (replace_arg var_stack') args)) :: acc, var_stack'')

        | Call (None, fname, args) ->
          ((Call (None, fname,
                  List.map (replace_arg var_stack') args)) :: acc, var_stack')

        | Ret (Some arg) ->
          ((Ret (Some (replace_arg var_stack' arg))) :: acc, var_stack')

        | Ret None -> ((Ret None) :: acc, var_stack')

        | Print args ->
          ((Print (List.map (replace_arg var_stack') args)) :: acc, var_stack')

        | Nop -> (Nop :: acc, var_stack')

        | Phi (dest, pairs) ->
          let dest', var_stack'' = replace_dest dest var_stack' in
          ((Phi (dest', List.map (fun (label, arg) ->
               (label, replace_arg var_stack' arg)) pairs)) :: acc, var_stack'')

      end
      ([], var_stack)
      instrs
    |> fun (instrs', var_stack') ->
    (List.rev instrs', var_stack') in

  let blocks' =
    Core.String.Map.change
      blocks
      block
      ~f:begin function
        | Some instrs -> Some instrs'
        | None -> failwith ("missing block: " ^ block)
      end in

  let blocks'' =
    List.fold_left
      begin fun acc succ ->
        Core.String.Map.change
          acc
          succ
          ~f:begin function
            | Some instrs ->
              Some begin
                List.map
                  begin fun instr -> match instr with
                    | Phi ((var, typ), pairs) ->
                      let binding =
                        (block, Core.String.Map.find_exn var_stack' var
                                |> List.hd) in
                      let pairs' = binding :: List.remove_assoc block pairs in
                      Phi ((var, typ), pairs')
                    | _ -> instr
                  end
                  instrs
              end
            | None -> failwith ("missing block: " ^ succ)
          end
      end
      blocks'
      (Core.String.Map.find_exn succs block) in

  let blocks''', var_stack'' =
    Core.String.Set.fold
      (Core.String.Map.find_exn dt block)
      ~init:(blocks'', var_stack')
      ~f:(fun (acc, var_stack'') block' ->
          rename_variables block' var_stack'' acc dt succs) in

  (* we need to include the new variables *)
  let var_stack''' =
    Core.String.Map.merge
      var_stack''
      var_stack
      ~f:begin fun ~key -> function
        | `Both (_, original) -> Some original
        | `Left lst -> Some [List.rev lst |> List.hd]
        | `Right _ -> failwith "should be impossible"
      end in

  (blocks''', var_stack''')

let to_ssa
    (func : Bril.Func.t)
  : Bril.Func.t =

  let open Bril.Func in

  let defined_vars = vars func in
  let df = dominance_frontier func in
  let dt = dominance_tree func in

  (* first pass: insert phi-nodes *)

  let blocks' =
    Core.String.Set.fold
      defined_vars
      ~init:func.blocks
      ~f:begin fun acc var ->
        insert_phi_nodes var (defs func var) acc df
      end in

  (* second pass: rename variables *)

  let var_stack =
    Core.String.Set.fold
      defined_vars
      ~init:Core.String.Map.empty
      ~f:(fun acc var -> Core.String.Map.add_exn acc ~key:var ~data:[var]) in

  let blocks'', _ =
    List.fold_left
      (fun (acc, var_stack') block -> rename_variables block var_stack' acc dt func.succs)
      (blocks', var_stack)
      (Core.String.Map.keys blocks') in

  {func with blocks = blocks''}

let from_ssa
    (func : Bril.Func.t)
  : Bril.Func.t =

  let open Bril.Func in
  let open Bril.Instr in

  failwith "todo"

  (* it's actually not so simple... sad :( *)

  (* have to insert to blocks *)

  (* let blocks' =
   *   Core.String.Map.fold
   *     func.blocks
   *     ~init:func.blocks
   *     ~f:begin fun ~key ~data acc ->
   *       let blocks'', instrs =
   *         List.fold_left
   *           begin fun (acc', lst) -> function
   *             | Phi ((var, typ), pairs) ->
   *               (List.fold_left
   *                  begin fun acc'' (label, var') ->
   *                    Core.String.Map.change
   *                      acc''
   *                      label
   *                      ~f:begin function
   *                        | Some instrs ->
   *                          Some (instrs @
   *                                [Unary ((var, Bril.Bril_type.IntType),
   *                                        Bril.Op.Unary.Id, var')])
   *                        | None -> failwith "missing block"
   *                      end
   *                  end
   *                  acc'
   *                  pairs, lst)
   *             | Label label -> (acc', (Label label) :: lst)
   *             | instr -> (acc', instr :: lst)
   *           end
   *           (acc, [])
   *           data in
   *       Core.String.Map.change
   *         blocks''
   *         key
   *         ~f:begin function
   *           | Some instrs -> Some (List.rev instrs)
   *           | None -> failwith "missing block"
   *         end
   *     end in
   * 
   * {func with blocks = blocks'} *)
