(*Problem 1.
Write a function clone of type 'a * int -> 'a list. The function takes an item e and a nonnegative integer n and returns a list containing n copies of e. For example, clone("hi", 4) returns ["hi"; "hi"; "hi"; "hi"].
*)
let rec clone ((e, n) : 'a * int) : 'a list =
		match n with 
         0 -> []
        |_ -> e :: clone(e, n-1)

 (*Problem 2.

Write a function rev that reverses the elements in a list.
*)

let rec rev (l: 'a list) : 'a list =
	match l with
     [] -> []
    |first::rest-> rev(rest)@[first]


(*
Problem 3.

The naive algorithm for reversing a list takes time that is quadratic in the size of the argument list. In this problem, you will implement a more efficient algorithm for reversing a list: your solution should only take linear time. Call this function fastRev. The key to fastRev is that it builds the reversed list as we recurse over the input list, rather than as we return from each recursive call. This is similar to how an iterative version of list reversal, as implemented in a language like C, would naturally work.

To get the right behavior, your fastRev function should use a local helper function revHelper to do most of the work. The helper function should take two arguments: (1) the suffix of the input list that remains to be reversed; (2) the reversal of the first part of the input list. The helper function should return the complete reversed list. Therefore the reversal of an input list l can be performed via the invocation revHelper(l, []). I've already provided this setup for you, so all you have to do is provide the implementation of revHelper (which is defined as a nested function within fastRev) and invoke it as listed above. The call (fastRev (clone(0, 10000))) should be noticeably faster than (rev (clone(0, 10000))).
*)

let fastRev (l : 'a list) : 'a list =
    let rec revHelper (remain, sofar) =
    	 match remain with
          [] -> sofar
         | h::rest -> revHelper (rest,(h::sofar))

in revHelper(l, [])

(*• Problem 4.

Write a function tails of type 'a list -> 'a list list that takes a list and returns a list of lists containing the original list along with all tails of the list, from longest to shortest. For example, tails [1;2;3] is [[1;2;3];[2;3];[3];[]].
*)

let rec tails (l : 'a list) : 'a list list =
	match l with
	 [] -> [[]]
    |h::rest -> l::(tails rest)


 (*Problem 5.

Write a function to return the second-to-last element of a list. To deal with the case when the list has fewer than two elements, the function should return a value of the built-in option type, defined as follows:

type 'a option = None | Some of 'a 

Examples:
# penultimate [1;2;3];;
- : int option = Some 2
# penultimate ["a"];;
- : string option = None
*)
let rec penultimate (l: 'a list) : 'a option =
	match l with
    	[] -> None
        |[_] -> None
        |a::b::rest -> if rest = [] then Some a else penultimate (b::rest)

(*Problem 6.

Flatten a list of lists.

Examples:
# flatten [[2]];;
- : int list = [2]
# flatten [[2]; []; [3;2]];;
- : int list = [2; 3; 2]
*)

let rec flatten (l: 'a list list) : 'a list =
	match l with
    	[] -> []
        |[[]] -> []
        |h::rest -> h@flatten rest
(*Problem 7.

Convert a list of digits (assumed to be numbers between 0 and 9) into an integer. You may assume that the first element of the list is not 0.

Examples:
# intOfDigits []
- : int = 0
# intOfDigits [3;1;0;2]
- : int = 3102
*)

let rec intOfDigits (l: int list) : int =
	let rec intOfDigitsHelper(remain, n) : int=
		match remain with
    		[] -> n
        	|h::rest -> intOfDigitsHelper(rest, h+n*10)
    in intOfDigitsHelper(l,0)

let intOfDigits l = List.fold_left (fun remain x -> remain*10 + x ) 0 l

(*Problem 8.

Merge two sorted lists of integers into a single sorted list. You may assume that the given lists are both sorted already. Don't define any helper functions.

Example:
# merge ([1;3;5], [2;4])
- : int list = [1;2;3;4;5]
*)
let rec merge l1 l2 =
    match l1 with
        [] -> l2
        |x1::xs1 -> match l2 with
            [] -> l1
            |x2::xs2 -> if x1<=x2 then x1:: (merge xs1 l2) else x2::merge (l1 xs2)
let rec merge ((l1, l2): int list * int list) : int list =
 match l1 with
    	[] -> l2
      | h1::t1 -> match l2 with
        			[] -> l1
                  | h2::t2 -> if h1<h2 then
                  	h1::merge(t1,l2) else
                    h2::merge(l1,t2)

(*Problem 9.

Implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N, E) where N is the number of duplicates of the element E. Don't define any helper functions.

Examples:
# encode [];;
- : (int * 'a) list = []
# encode ["a"];;
- : (int * string) list = [(1, "a")]
# encode ["a";"a"];;
- : (int * string) list = [(2, "a")]
# encode ["a";"b";"b"];;
- : (int * string) list = [(1, "a"); (2, "b")]
# encode ["a";"b";"b";"a";"a"];; 
- : (int * string) list = [(1, "a"); (2, "b"); (2, "a")]
*)

let rec encode (l: 'a list) : (int * 'a) list =
	match l with
    	[] -> []
        |h::rest -> if rest = [] then [(1,h)] else
        			let remain = encode rest in
                    	match remain with
                        	(n,m)::rest2 -> if h = m then (n+1, m)::rest2
                            				else (1,h)::remain


(*Problem 10.

Rotate a list n places (i.e., take n elements off the front and move them to the back). You may assume n is between 0 and the length of the list, inclusively. Do not define any helper functions.*)

let rec rotate ((l, n) : 'a list * int) : 'a list = 
	match n with
    	0 -> l
        |_->match l with
        	h::rest->rotate(rest@[h], n-1)

 (* Problem 11.

Convert a (decimal) integer into binary by encoding it as a list of integers. You may use the builtin modulo operator mod : int * int -> int. Don't define any helper functions.

Examples:
# dec2bin 0;;
- : int list = [0]
# dec2bin 1;;
- : int list = [1]
# dec2bin 2;;
- : int list = [1; 0]
# dec2bin 3;;
- : int list = [1; 1]
# dec2bin 4;;
- : int list = [1; 0; 0]
# dec2bin 15;;
- : int list = [1; 1; 1; 1])
*)
let rec dec2bin (n: int) : int list = 
	match n with 
    	0 -> [0]
        |1-> [1]
        |_-> dec2bin(n/2)@[n mod 2]

(*Problem 12.

Write a function pairify of type 'a list -> ('a * 'a) list that takes a list and pairs up consecutive elements of the list. 
If the list has an odd length, then the last element should be dropped from the result. Do not define any helper functions.

Examples:

# pairify [1;2;3;4];;
- : (int * int) list = [(1, 2); (3, 4)]
# pairify [1;2;3;4;5];;
- : (int * int) list = [(1, 2); (3, 4)]
*)
let rec pairify (l : 'a list) : ('a * 'a) list =
	match l with
    	[] -> []
        |h1::rest -> if rest = [] then [] else match rest with 
        				h2::rest2 -> [(h1,h2)]@pairify rest2

