
let string_of_string_list lst =
  "[" ^ String.concat "; " lst ^ "]"

let string_of_string_set set =
  Core.String.Set.to_list set |> string_of_string_list

let string_of_string_set_string_map map =
  Core.String.Map.fold
    map
    ~init:[]
    ~f:begin
      fun ~key ~data acc ->
        (key ^ ": " ^
         (Core.String.Set.to_list data |> string_of_string_list))
        :: acc
    end
  |> List.rev
  |> String.concat "\n"
