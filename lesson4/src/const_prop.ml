
open Data_flow

module ConstPropAnalysis  = struct
  type value =
    | VConst of Bril.Const.t
    | VBottom (* we don't know what the value is *)

  type workset = value Core.String.Map.t

  let direction = Forward

  let const_eq a b =
    let open Bril.Const in
    match a, b with
    | Int a', Int b' -> a' = b'
    | Bool a', Bool b' -> a' = b'
    | _ -> false

  let meet =
    Core.String.Map.merge
      ~f:(fun ~key -> function
          | `Both (VConst v, VConst v') when const_eq v v' -> Some (VConst v)
          | `Left (VConst v) | `Right (VConst v) -> Some (VConst v)
          | _ -> Some VBottom)

  let transfer workset (cfg_node : workset cfg_node) =
    let open Bril.Instr in
    match cfg_node.instr with
    | Const ((var, _), v) ->
      Core.String.Map.update workset var ~f:(fun _ -> VConst v)
    | Unary ((var, _), Bril.Op.Unary.Id, arg) ->
      begin
        match Core.String.Map.find workset arg with
        | Some v -> Core.String.Map.update workset var ~f:(fun _ -> v)
        | None -> workset
      end
    | Unary ((var, _), _, _)
    | Binary ((var, _), _, _, _)
    | Call ((Some (var, _)), _, _) ->
      Core.String.Map.update workset var ~f:(fun _ -> VBottom)
    | _ -> workset

  let compare : workset -> workset -> int = Core.String.Map.compare Stdlib.compare

  let string_of_val = function
    | VConst (Bril.Const.Int i) -> string_of_int i
    | VConst (Bril.Const.Bool b) -> string_of_bool b
    | VBottom -> "?"

  let print workset =
    Core.String.Map.fold workset ~init:[]
      ~f:(fun ~key:var ~data:v acc -> (var ^ ": " ^ string_of_val v) :: acc)
    |> List.rev |> String.concat ", "
end

module ConstPropCfgConstructor = CfgConstructor (ConstPropAnalysis)
module ConstPropDataFlow = DataFlow (ConstPropAnalysis)

let perform
    (func : Bril.Func.t)
  : ConstPropAnalysis.value Core.String.Map.t cfg_node list =

  let top = Core.String.Map.empty in
  let cfg = ConstPropCfgConstructor.construct_cfg top func in
  ConstPropDataFlow.perform_analysis top cfg;
  cfg

let print_results (cfg : ConstPropAnalysis.value Core.String.Map.t cfg_node list) : unit =
  List.iter
    (fun cfg_node ->
       let instr = Bril.Instr.to_json cfg_node.instr
                   |> Yojson.Basic.to_string in
       let printed = ConstPropAnalysis.print cfg_node.workset in
       instr ^ "  ->  " ^ printed |> print_endline)
    cfg
