let my_subset_test1 = not (subset [2;4] [1;2;3])
let my_subset_test2 = subset [1;2;2;3;3] [1;2;3]
let my_subset_test3 = not (subset [2;3;4] [3;4])

let my_equal_sets_test0 = equal_sets [2;3;4;3] [4;2;3]
let my_equal_sets_test1 = not (equal_sets [1;2;3] [2;3;4])

let my_set_union_test0 = equal_sets (set_union [1;3;5] []) [1;3;5]
let my_set_union_test1 = equal_sets (set_union [1;2;3;4] [4;2]) [4;3;2;1]

let my_set_intersection_test0 = equal_sets (set_intersection [2;4;6;8] [8;4]) [4;8]
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [4;3;2;1]) [1;2;3]

let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4] [5;6]) [1;2;3;4]
let my_set_diff_test1 = equal_sets (set_diff [1;2] []) [1;2]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x *. 2.) 3. = infinity

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x *. 2. +. 1.) 0 1. = 1.

type transportation_nonterminals = 
	| Car | Sedan | Coupe | Convertible | Bike | Train | Plane
let transporation_rules = 
	[Car, [N Sedan; N Coupe; N Convertible];
	 Sedan, [T"4_doors"];
	 Coupe, [T"2_doors"];
	 Convertible, [T"open_hood"];
	 Bike, [T"2_wheels"];
	 Bike, [T"3_wheels"];
	 Train, [T"Run_on_a_track"];
	 Plane, [T"Fly_in_the_sky"]]

let transportation_grammar = Car, transporation_rules

let my_transportation_test0 = filter_blind_alleys transportation_grammar = transportation_grammar
let my_transportation_test1 = filter_blind_alleys (Car, [Car, [N Sedan; N Coupe; N Convertible];
	 Sedan, [T"4_doors"];
	 Coupe, [T"2_doors"];
	 Bike, [T"2_wheels"];
	 Bike, [T"3_wheels"];
	 Train, [T"Run_on_a_track"]]) = (Car, [Sedan, [T"4_doors"];
	 Coupe, [T"2_doors"];
	 Bike, [T"2_wheels"];
	 Bike, [T"3_wheels"];
	 Train, [T"Run_on_a_track"]])
	