(*Problem 1 test*)
let my_subset_test0 = subset [] [0;4;7;10]
let my_subset_test1 = subset [5;1;7] [7;5;1]
let my_subset_test2 = subset [5;1;7;7] [7;5;1]
let my_subset_test3 = not (subset [1;3;7] [])
let my_subset_test3 = not (subset [1;3;7] [2;3;7;10])

(*Problem 2 test*)
let my_equal_sets_test0 = equal_sets [5;1;7] [7;7;1;5;5;5]
let my_equal_sets_test1 = not (equal_sets [5;3;4] [3;7;3;5;4])

(*Problem 3 test*)
let my_set_union_test0 = equal_sets (set_union [] [5;6;7]) [5;6;7]
let my_set_union_test1 = equal_sets (set_union [3;1;3;7] [1;2;3]) [1;2;3;7]
let my_set_union_test2 = equal_sets (set_union [] []) []
let my_set_union_test2 = equal_sets (set_union [1] []) [1]     

(*Problem 4 test*)
let my_set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1]
let my_set_intersection_test3 =
  equal_sets (set_intersection [1;2;3;4] [5;5;6;7;8]) []

(*Problem 5 test*)
let my_set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4]
let my_set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let my_set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []
let my_set_diff_test4 = equal_sets (set_diff [] []) []

(*Problem 6 test*)
let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let my_computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
let my_computed_fixed_point_test4 = computed_fixed_point (=) (fun x -> x * 3 - 2) 1 = 1

(*Problem 7 test*)
let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let my_computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.
let my_computed_periodic_point_test2 =
  computed_periodic_point (=) (fun x -> x * 2 - 1) 1 (1) = 1

(*Problem 8 test*)
let my_while_away_test0 = equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9]

(*Problem 9 test*)
let my_rle_decode_test0 = equal_sets (rle_decode [2,0; 1,6]) [0;0;6]
let my_rle_decode_test1 = equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"]) ["w"; "w"; "w"; "x"; "z"; "z"]
let my_rle_decode_test0 = equal_sets (rle_decode [0,3]) []

(*Problem 10 test*)
type awksub_nonterminals =
  S | NP | VP | V | D | No

let awksub_rules =
   [S, [T"Hi"; N NP; N VP];
    NP, [N D; N No];
    VP, [N V ; N NP];
    No, [T"cat"];
    No, [T"dog"];
    V, [T"ate"];
    V, [T"bit"];
    D, [T"the"];
    D, [T"a"]]

let awksub_grammar = S, awksub_rules

let my_awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar

let filtered_rules =
   [No, [T"cat"];
    No, [T"dog"];
    V, [T"ate"];
    V, [T"bit"];
    D, [T"the"];
    D, [T"a"]]

let my_awksub_test1 =
  filter_blind_alleys (S, List.tl (List.tl awksub_rules)) = (S, filtered_rules)
