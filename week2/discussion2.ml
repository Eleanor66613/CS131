discussion2.ml

(*
when to use List.map and when to use List.fold_left

if want to apply a function to the list of every element, then use List.map; 
List.fold_left: take the list and then return a value *)

let sumLst l =
	List.fold_left (function x y -> x + y ) 0 l

let sumLst l =
	List.fold_left (+) 0 l

encode: ["a"; "b"; "b"]
=> [(1, "a"), (2, "b")]

do it by List.fold_right

(* O(n) space *)
let encode l =
	List.fold_right (function x y -> if x = y)
(*const O(1) space *)
let sum l =
	let rec sumh a r =
		match r with
			[] -> a
			|h::t -> sumh (a+h) t
	in sumh 0 l 

(*fold_left is O(1) and fold_right is O(n), so should always try to use list.fold_left instead of list.fold_right*)

fold_left (-) 0 [1;2;3]
(((0-1)-2)-3)
fold_right (-) [1;2;3] 0
1-(2-(3-0))





