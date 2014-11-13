let contins3 e l =
	List.fold_left
		(fun recursiveResult x -> x = e || recursiveResult)
	l false

let contains2 e l =
	List.fold_right
		(fun x recursiveResult -> x = e || recursiveResult)
	false l

let indexOf2 e l =
	let (_, result) =
		List.fold_left
			(fun (i, res) x ->
				match res with
					None -> if x = e then (i+1, Some(i+1)) else (i+1, None)
				   |Some _-> (i,res))
			(0, None) l
	in result

(* scoping:
	when a variable is used ( or referenced), which variable declaration does it refer to?
	
	* lexical* or *static* scoping: the variable referred to is always the declaration of that variable that nearest in the enclosing scope

*)

let x = 5
(* x is in scope for the rest of the file *)
let double n = n*2
(* double in scope for the rest of the file, n is in scope just in the body of double *)

let rec fact n=
	match n with
		0 -> 1
		| i -> i * fact(i-1) (* if no "rec", fact will go up and look for other "fact" in the file declared before *)

(*i is in scope just for that case of the pattern match *)
(* fact is in scope in its own definitaion and for the rest of the file is add "rec" before it, otherwise it is
just in the scope for the rest of the file, but not in its defination *)

let x = 34
in x + 54
(*x is in scope just in the expression after "in" *)

let rec x = x +1;
Error: this kind of expression is not allowed.

let x = 3;
let f y = x+y
val f: int -> int = <fun>
f 45;;
-: int = 48;;
let x = 0;
f 45
-; int = 48;;

(* x is always 3 in f function because f function will only look up for the x in scope, even if x is redefinioned later, it won't effect *)

let add x y = x +y;;
is short for :
let add = (function x -> (function y -> x +y))
let addTow = add 2
#addTow 8 
-int : 10

let x = x+1
in x + 54
(* x is in scope just in the expression after "in" *)

(* Types
	what is a type?
	-a set of values that have a shared set of operations
example: int is the set of 32-bit integers operations : +, -, *, >, etc *)

int List
[], [1;2;3]
::, @

int * bool
(1, true)
access components (by pattern matching)

int -> bool
(function x -> x > 0)

(*several dimensions on which to evaluate type systems:
1. Static vs. dynamic typechecking
		(ocaml is static)
	-static means "at compile time" （different from python)
		-before execution
	-give each expression in the program a type
		-guarantee: if E has type T then at run time the value of E (if E's evaluation terminates normally)will be a member of the set T)
	advantages:
		-catch errors early
			checking all possible executions of a function

		-faster at run time
			-no need to re-check types at run time
			-allows some compiler optimizations
		-program documentation 
	disadvantages:
		-slower compilation
		-restrictive ( conservative)
			-can reject your program even if it's not buggy
	*)

type intorstring = I of int | S of string
[I 1; S "hi"; S "type"; I 34];;
-: intorstring list = [I 1; S "hi"; S "type"; I 34]





























