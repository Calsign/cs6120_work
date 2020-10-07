
let main channel =
  Yojson.Basic.from_channel channel
  |> Bril.from_json
  |> List.iter (fun func ->
      Const_prop.perform func
      |> Const_prop.print_results)

let () =
  match Sys.argv with
  | [| _ |] -> main stdin
  | [| _; fname |] -> open_in fname |> main
  | _ -> prerr_endline "Unexpected argument"
