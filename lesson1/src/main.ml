
open Ast
open Cfg

let main channel =
  let funcs = load_channel channel in
  (* something simple: list all of the labels *)
  List.iter (fun func ->
      let cfg_nodes = cfg_of_func func in
      List.iter (fun cfg_node ->
          List.iter (fun label ->
              print_endline ("label '" ^ label ^ "' in function '" ^ func.name ^ "'"))
            cfg_node.labels)
        cfg_nodes)
    funcs

let () =
  match Sys.argv with
  | [| _; fname |] -> open_in fname |> main
  | _ -> main stdin
