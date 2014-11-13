Ocaml programming problems:
Recursion
first-class functions (map, fold, etc)
pattern matching
datatypes

Concepts:(no exception)
static vs. dynamic typechecking
static scoping
parametric polymorphism
overloading

1.Ocaml use overloading for greater than and equal, but they cheat to make them look like polymorphism. (??????????how they cheat????????????)
2. try xxxxxx with xxxxxx -> ????????


# (=);;
- : 'a -> 'a -> bool = <fun>
# 3<4;;
- : bool = true
# 0.3<0.4;;
- : bool = true
# 03>4;;
- : bool = false

(*exceptions
	-implicitly propagate errors up the call stack
		-good: simplifies your code
			-only the code that needs to handle the ecepton does something special
			-all other code just deals with the normal case
		-bad: easy to forget the check for an exception

another benefic: can use them to fail early
*)

let inc x = x +1

exception NegativeError of int

let incIfNonNeg l =
	try
		List.map(function x -> if x < 0 then raise NegativeError x else (inc x)) l
	with
		NegativeError n -> []
(*????????????????? how about the homework??????????????? *)




