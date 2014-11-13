CS131 class 3

(====Aquamacs====)

(* a new type with a finite set of values *)
type sign = Positive | Negative | Zero

let signof n =

(* datatypes allow users to define their own data structures generalization of:
	-enums
	-structs
	-unions
*)
	match n with 
	0 -> Zero
      | _when n>0 -> Positive
     | _->Negative

let signToInt s =
	match s with
	 Zero -> 0
	|Negative -> -1
	|Positive ->1

(* sepecific type have to be capital, type name dont neet to *)


type point = Cartesian of (float * float) | Polar of (float * float)
(Cartesian is a constructor of point)
# Cartesian (3.0, 4.0);;

#int ;;
Error: unbound value int
#point;;
Error: unbound value int

#let p = Cartesian (3.0, 4.0);;
val p : point = Cartesian (3, 4)

let negate p =
	match p with
	 	Cartesian (x, y) -> (-.x, -.y)
 	|Polar (rho, theta) -> if theta > 360...... else Polar(rho, theta + .180.0)
 	(cannot do the restriction in type defination)

 let normalize p =
 	mathc p with
 		Cartesian _->p
 		|Polar (rho, theta) -> Polar (rho, theta mod 360)

 let createPolar (rho, theta) =
 	Polar(rho, theta mod 360)

 let toPolar p -
 	match p with
 	Polar _ -> expr
 	| Cartesian (x, y) -> Polar (sqrt(x*.x +. y*.y), atan(y/.x))

 (* 
 	toPolar (Cartesian(3.0, 4.0));;
 *)

 type optionalInt = None | Some of int
 type 'a optionalInt = None | Some of 'a
 (* 'a  is like generic in C++ *)

 # let safeDiv (x, y) =
 	if y = 0 then None else Some(x/y)

 (* recursive types *)
 type intlist = Nil | Cons of (int * intlist)

 let rec length l =
 	match l with
 		Nil -> 0
 		|Cons(_, rest) -> 1+ (length rest)

(* data only at internal nodes *)
 type binaryTree = Leaf | Node of (int * binaryTree * binaryTree)

 let tree = Node (1, Node (2, Leaf, Leaf), Node(3, Node (4, Leaf,Leaf), Leaf))
(* data only at leaves *)
 type 'a binaryTree = Leaf of 'a | Node of 'a binaryTree * 'a binaryTree

 let rec preorder tree =
 	match tree with
 		Leaf -> []
 		|Node (a,leftChild,rightChild) -> [a]@( precorder leftChild )@( preorder rightChild )

 (* insert a binary search tree *)

 let rec insert (n, t) =
 	match t with
 		Leaf -> Node (n, Leaf, Leaf)
 		|Node (a, left, right) -> 
 			if n < a then Node ( a, insert(n, left), right)
 			else Node (a, left, insert (n, right))

(* functions *)
(* functions are *first-class*;
		-they are full-fledged expressions in the language
		-you can pass them to other functions
		-you can return them from other functions
*)

let square x = x*x

let toThe4th x = square (square x)

let fourthRoot x = sqrt(sqrt x)
(*twice is a higher order function, which takes a function as an argument *)
let twice(f,x) = f(f x)

#twice(sqrt, 81.9);;
-: float = 3.

#let toThe4th : int -> int = <fun>

#toThe4th 4;;
-: int = 256

let ncalls(f, x, n) =
	match n with
	0 -> x
	|_-> f(ncalls ( f, x, n-1))

let ncalls(f, x, n) =
	match n with
	0 -> x
	|_-> 
		let rest = ncalls(f, x, n-1)
		in f rest

#ncalls ((function x -> x*x), 2, 4)

(* anonymous functions
	aka, lambdas, closures

	syntax: function pat -> exp ???????

	let f x = x*x is shorthand for
	let f = (function x -> x*x)

*) 

let f y = y*y
val f: int->int =<fun>

let f = (function y -> y*y);;
val f: int -> int = <fun>

let swap = (function (x, y) -> (y,x))
;;
let swap (x, y ) = (y, x)

(* rec has to be written here *)

let rec fact n =
		match n with
			0_. 1 | _-> n * fact(n-1);;

let rec fact =
	(function n ->
		match n with 0 -> 1 | _->n * fact(n-1));;













 














































