
open Printers
open Dom

let common channel =
  Yojson.Basic.from_channel channel
  |> Bril.from_json

let main_dominators channel =
  common channel
  |> List.iter
    begin fun func ->
      print_endline ("dominators for function " ^ func.Bril.Func.name ^ ":");
      print_endline (dominators func |> string_of_string_set_string_map);
    end

let main_dominance_tree channel =
  common channel
  |> List.iter
    begin fun func ->
      print_endline ("dominance tree for function " ^ func.Bril.Func.name ^ ":");
      print_endline (dominance_tree func |> string_of_string_set_string_map);
    end

let main_dominance_frontier channel =
  common channel
  |> List.iter
    begin fun func ->
      print_endline ("dominance frontier for function " ^ func.Bril.Func.name ^ ":");
      print_endline (dominance_frontier func |> string_of_string_set_string_map);
    end

let () =
  match Sys.argv with
  | [| _; "--dominators" |] -> main_dominators stdin
  | [| _; "--dominance-tree" |] -> main_dominance_tree stdin
  | [| _; "--dominance-frontier" |] -> main_dominance_frontier stdin
  | [| _ |] -> prerr_endline "Missing argument"
  | _ -> prerr_endline "Unexpected argument"
