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
let rec rec_factorial n =
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