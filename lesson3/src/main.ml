
let main_dce channel =
  Yojson.Basic.from_channel channel
  |> Bril.from_json
  |> Dce.perform_funcs (* perform dead code elimination *)
  |> Bril.to_json
  |> Yojson.Basic.to_channel stdout

let main_lvn channel =
  Yojson.Basic.from_channel channel
  |> Bril.from_json
  |> Dce.perform_funcs (* perform dead code elimination first *)
  |> Lvn.perform_funcs (* perform local value numbering *)
  |> Bril.to_json
  |> Yojson.Basic.to_channel stdout

let () =
  (* this isn't pretty but whatever *)
  match Sys.argv with
  | [| _ |] -> prerr_endline "Please specify either --dce or --lvn"
  | [| _; fname; "--dce" |]
  | [| _; "--dce"; fname |] -> open_in fname |> main_dce
  | [| _; "--dce" |] -> main_dce stdin
  | [| _; fname; "--lvn" |]
  | [| _; "--lvn"; fname |] -> open_in fname |> main_lvn
  | [| _; "--lvn" |] -> main_lvn stdin
  | _ -> prerr_endline "Unexpected argument"
