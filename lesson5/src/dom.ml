
let multi_intersect sets =
  List.fold_left
    begin fun acc set ->
      match acc with
      | Some acc' -> Some (Core.String.Set.inter acc' set)
      | None -> Some set
    end None sets

let set_map_inverse
    (set_map : Core.String.Set.t Core.String.Map.t)
  : Core.String.Set.t Core.String.Map.t =
  Core.String.Map.fold
    set_map
    ~init:Core.String.Map.empty
    ~f:begin fun ~key ~data acc ->
      Core.String.Set.fold
        data
        ~init:acc
        ~f:(fun acc' dom -> Core.String.Map.add_multi acc' ~key:dom ~data:key)
    end
  |> Core.String.Map.map ~f:Core.String.Set.of_list

let include_keys
    (keys : Core.String.Set.t)
    (set_map : Core.String.Set.t Core.String.Map.t)
  : Core.String.Set.t Core.String.Map.t =
  Core.String.Map.merge
    set_map
    (Core.String.Set.fold
       keys
       ~init:Core.String.Map.empty
       ~f:(fun acc key ->
           Core.String.Map.add_exn acc key Core.String.Set.empty))
    ~f:begin fun ~key -> function
      | `Both (v, _) -> Some v
      | `Left v | `Right v -> Some v
    end

let dominators
    (func : Bril.Func.t)
  : Core.String.Set.t Core.String.Map.t =

  (* start out with every node dominated by every other *)
  let blocks = Core.String.Map.key_set func.blocks in
  let init = Core.String.Map.map func.blocks ~f:(fun _ -> blocks) in

  let rec aux (map : Core.String.Set.t Core.String.Map.t) =
    let map' = Core.String.Map.mapi
        func.blocks
        ~f:begin
          fun ~key:block ~data:_ ->
            let inter =
              Core.String.Map.find_exn func.preds block
              |> List.map (Core.String.Map.find_exn map)
              |> multi_intersect
              (* default to empty set if no predecessors *)
              |> Option.value ~default:Core.String.Set.empty in
            Core.String.Set.singleton block
            |> Core.String.Set.union inter
        end in

    if Core.String.Map.equal Core.String.Set.equal map map'
    then map else aux map' in

  aux init

let strict_dominators
    (func : Bril.Func.t)
  : Core.String.Set.t Core.String.Map.t =

  dominators func

  |> Core.String.Map.mapi
    ~f:begin fun ~key ~data ->
      Core.String.Set.filter data ~f:((<>) key)
    end

let dominance_tree
    (func : Bril.Func.t)
  : Core.String.Set.t Core.String.Map.t =

  let aux data dom =
  Core.String.Set.fold
    data
    ~init:Core.String.Set.empty
    ~f:begin fun acc d ->
        Core.String.Map.find_exn dom d
        |> Core.String.Set.union acc
    end in

  let dom = strict_dominators func in

  Core.String.Map.mapi
    dom
    ~f:(fun ~key ~data ->
        Core.String.Set.diff data (aux data dom))

  |> set_map_inverse

  |> include_keys (Core.String.Map.key_set func.blocks)

let dominance_frontier
    (func : Bril.Func.t)
  : Core.String.Set.t Core.String.Map.t =

  let pred_doms key dom =
  Core.String.Set.fold
    (Core.String.Map.find_exn func.preds key |> Core.String.Set.of_list)
    ~init:Core.String.Set.empty
    ~f:begin fun acc d ->
        Core.String.Map.find_exn dom d
        |> Core.String.Set.union acc
    end in

  let dom = dominators func in

  Core.String.Map.mapi
    dom
    ~f:(fun ~key ~data ->
        Core.String.Set.diff (pred_doms key dom) data)

  |> set_map_inverse

  |> include_keys (Core.String.Map.key_set func.blocks)
