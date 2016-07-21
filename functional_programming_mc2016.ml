(* http://nicf.net/static/mc2016/ocaml.zip *)

(* Maps *)
let rec map xs ~f =
	match xs with
	| [] -> []
	| y::ys -> (f y) :: (map ys ~f)
;;

(* Filters *)
let rec filter xs ~f = 
	match xs with
	| [] -> []
	| y::ys -> if f y then y::(filter ys ~f) else (filter ys ~f)
;;

(* What is Option.join :o *)
(* Option.join;; -> 'a option option -> 'a option = <fun> *)
let my_join x =
	match x with
	| None -> None
	| Some None -> None
	| Some (Some y) -> Some y
;;

(* function_exn for function that throws exception instead of returning option *)

(* Folding *)
(* List.fold;; -> 'a list -> init: 'accum -> f:('accum -> 'a -> 'accum) -> 'accum = <fun> *)
let found_a_three xs = List.fold xs ~init:false ~f:(fun found x -> found || x = 3);;

(* Problem 1: Factorial (folding) *)
let fold_factorial n = List.fold(List.range 1 n + 1) ~init:1 ~f:(fun x y -> x * y);;

(* recursively *)
let rec_factorial n =
	let rec factorial' n product =
	if n = 0
	then product
	else factorial' (n - 1) (n * product)
in factorial' n 1
;;

(* Problem 2: Getting a range *)
let range i j step = List.filter(List.range i j) ~f:(fun n -> (n - i) % step = 0);;

(* Problem 4*)
(* a: Quicksort *)
let rec qsort nums =
	match nums with
	| [] -> []
	| first::rest -> let low, high = List.partition_tf rest ~f:(fun n -> n < first) in qsort low @ [first] @ qsort high
;;

let rec rev_qsort nums =
	match nums with
	| [] -> []
	| first::rest -> let low, high = List.partition_tf rest ~f:(fun n -> n < first) in rev_qsort high @ [first] @ rev_qsort low
;;

(* b: Mergesort *)
(* The merge function *)
let rec merge list1 list2 ~cmp =
	match list1, list2 with
	| something, [] -> something
	| [], something -> something
	| e1::rest1, e2::rest2 -> if (cmp e1 e2) <= 0 then e1::(merge rest1 list2 ~cmp) else e2::(merge list1 rest2 ~cmp)
;;

(* The sorting *)
let rec msort nums =
	match nums with
	| [] -> []
	| [lonely] -> [lonely]
	| _ -> let part1, part2 = List.split_n nums (List.length nums / 2) in
	merge ~cmp:(fun x y -> if x < y then -1 else if x = y then 0 else 1) (msort part1) (msort part2)
;;

(* Problem 5 *)
(* a: subsets *)
let rec prepend_all prepender prependee =
	match prependee with
	| [] -> []
	| first::rest -> (prepender::first)::(prepend_all prepender rest)
;;

let rec subsets things =
	match things with
	| [] -> [[]]
	| first::rest -> (prepend_all first (subsets rest)) @ (subsets rest)
;;

(* b: subsets of some length *)
(* this isn't polynomial time *)
let choose length nums = List.filter (subsets nums) ~f:(fun subset -> (List.length subset = length));;

(* this is in polynomial time *)
let rec rec_choose length nums =
	match length, nums with
	| length, nums when (length > List.length nums) -> []
	| 0, anything -> [[]]
	| anything, first::rest -> (prepend_all first (rec_choose (length - 1) rest)) @ (rec_choose length rest)
;;

(* Problem 6 *)
(* a: Write a function that... *)
(* i: the same thing as List.tl *)
let my_tl list =
	match list with
	| [] -> None
	| first::rest -> Some rest
;;

(* ii: returns either the nth element of the list or None *)
let rec my_nth list index =
	match list, index with
	| list, index when (index >= List.length list) -> None
	| first::rest, 0 -> Some first
	| first::rest, index -> my_nth rest (index - 1)
;;

(* iii: reverses a list *)
let rec my_rev list =
	match list with
	| [] -> []
	| first::rest -> my_rev rest @ [first]
;;

(* iv: does the same thing as List.find_map *)
let rec my_find_map list ~f =
	match list with
	| [] -> None
	| first::rest -> 
		match f first with (* WHOA I CAN NEST MATCHES :O :O :O *)
		| None -> my_find_map rest ~f 
		| Some something -> Some something
;;

(* v: finds the max value in a list of integers *)
let maxval int_list = List.hd (rev_qsort int_list);;

(* vi: takes a list and element and 'intersperses' the element *)
let rec intersperse element list =
	match list with
	| [] -> []
	| [lonely] -> [lonely]
	| first::rest -> first::element::(intersperse rest element)
;;

(* vii: does the same thing as List.group *)
let my_group list ~break =
	let rec group' lst current meta =
		match lst with
		| [] -> []
		| [lonely] -> [[lonely]]
		| first::second::rest ->
			match (break first second), rest with
			| false, [] -> meta @ [current @ [first; second]]
			| true, [] -> meta @ [current @ [first]] @ [[second]]
			| false, _ -> group' (second::rest) (current @ [first]) meta
			| true, _ -> group' (second::rest) [] (meta @ [current @ [first]])
	in group' list [] []
;;

(* viii: does the same thing as List.partition_tf *)
let my_partition_tf list ~f =
	let rec partition_tf' lst yes no =
		match lst with
		| [] -> (yes, no)
		| first::rest ->
			match (f first) with
			| true -> partition_tf' rest (yes @ [first]) no
			| false -> partition_tf' rest yes (no @ [first])
	in partition_tf' list [] []
;;
