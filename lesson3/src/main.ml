
let main channel =
  let funcs = Yojson.Basic.from_channel channel |> Bril.from_json in
  let dce_funcs = Dce.perform_funcs funcs in
  Bril.to_json dce_funcs |> Yojson.Basic.to_channel stdout

let () =
  match Sys.argv with
  | [| _; fname |] -> open_in fname |> main
  | _ -> main stdin
