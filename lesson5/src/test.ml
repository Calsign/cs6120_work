
open OUnit2
open Printers
open Dom

let make_multi_intersect_test
    (name : string)
    (input : string list list)
    (output : string list option) =
  name >:: (fun _ ->
      assert_equal
        ~cmp:(Option.equal Core.String.Set.equal)
        ~printer:(function
            | Some set -> string_of_string_set set
            | None -> "none")
        (Option.map Core.String.Set.of_list output)
        (multi_intersect (List.map Core.String.Set.of_list input)))

let multi_intersect_tests = [
  make_multi_intersect_test "one set"
    [["a"; "b"; "c"]] (Some ["a"; "b"; "c"]);
  make_multi_intersect_test "empty"
    [] None;
  make_multi_intersect_test "two identical sets"
    [["a"; "b"]; ["a"; "b"]] (Some ["a"; "b"]);
  make_multi_intersect_test "two sets, intersection"
    [["a"; "b"]; ["b"; "c"]] (Some ["b"]);
  make_multi_intersect_test "two sets, empty intersection"
    [["a"]; ["b"]] (Some []);
  make_multi_intersect_test "three sets, different sizes"
    [["a"]; ["a"; "b"; "c"]; ["a"; "d"]] (Some ["a"]);
]

let make_set_map_inverse_test
    (name : string)
    (input : (string * string list) list)
    (output : (string * string list) list) =
  name >:: (fun _ ->
      assert_equal
        ~cmp:(Core.String.Map.equal Core.String.Set.equal)
        ~printer:string_of_string_set_string_map
        (Core.String.Map.of_alist_exn output
         |> Core.String.Map.map ~f:Core.String.Set.of_list)
        (Core.String.Map.of_alist_exn input
         |> Core.String.Map.map ~f:Core.String.Set.of_list
         |> set_map_inverse))

let set_map_inverse_tests = [
  make_set_map_inverse_test "basic"
    [("a", ["a"; "b"; "c"]); ("b", ["b"; "c"]); ("c", ["c"])]
    [("a", ["a"]); ("b", ["a"; "b"]); ("c", ["a"; "b"; "c"])];
  make_set_map_inverse_test "empty" [] [];
  make_set_map_inverse_test "one item"
    [("a", ["a"])] [("a", ["a"])];
  make_set_map_inverse_test "non-reflexive"
    [("a", ["b"])] [("b", ["a"])];
]

let suite =
  "test suite" >::: List.flatten [
    multi_intersect_tests;
    set_map_inverse_tests;
  ]

let _ = run_test_tt_main suite
