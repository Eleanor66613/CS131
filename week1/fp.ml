
let rec fact n =
  (* pattern matching *)
  match n with
      0 -> 1
    | _ -> n * (fact(n-1))

(* local variables:
   let x=e1 in e2 *)
let rec isEven n =
  let n = if n<0 then -n else n
  in
    match n with
	0 -> true
      | 1 -> false
      | _ -> isEven(n-2)

let rec isEven2 n =
  match n with
      0 -> true
    | 1 -> false
    | _ -> if n<0 then isEven2(n+2) else isEven2(n-2)

let rec isEven3 n =
  match n with
      0 -> true
    | 1 -> false
    | _ when n<0 -> isEven3(n+2)
    | _ -> isEven3(n-2)
	
let rec sumList l =
  match l with
      [] -> 0
    | first::rest -> first + (sumList rest)

(* grammar for the language

 E ::= C | X | let P=E in E | if E then E else E
     | match E with P -> E '|' P -> E ...
     | (E,E,E,...)

 P ::= C | _ | X | P::P | (P,P,...)
   
 C ::= integer, boolean, string constants
 X ::= variable names

 example: pattern to represent lists with at least 2 elements
     first::(second::rest)
   
*)   

(* everySecond [1;2;3;4;5] should return [2;4] *)	
let rec everySecond l =
  match l with
      [] -> []
    | [_] -> []
    | _::second::rest -> second::(everySecond rest)

(* how to write a recursive function:

   - one or more base cases

   - one or more recursive cases
      - make a recursive call
      - assume the result of the recursive call does what you want
      - use that result to build the answer for the entire thing   

*)
	
let rec everySecond2 l =
  match l with
      _::second::rest -> second::(everySecond rest)
    | _ -> []

(* what does this pattern match?  []::second::rest

   3::second::rest only matches [3;4;5]

   []::[3;_]::rest
   
   [[]; [3;4]]
   
*)

(* example: find the primes up to n

   top-down what do we need?

   1. check whether each element in {2..n} is prime

   2. check whether a given n is divisible by any number in
      {2..n}
   
*)


let isPrime n =
  let rec isPrimeHelper (n,i) =
    match n with
	1 -> false
      | 2 -> true
      | _ when i=n -> true
      | _ when (n mod i = 0) -> false
      | _ -> isPrimeHelper(n,i+1)
  in isPrimeHelper(n,2)
	
(* assume isPrime n exists *)	
let rec primesUpTo n =
  match n with
      1 -> []
    | _ when isPrime n -> n::(primesUpTo(n-1))
    | _ -> primesUpTo(n-1)
    

let add(x,y) = x+y
  
let three() = 3

(* zip([1;2;3], [4;5;6]) = [(1,4); (2,5); (3,6)] *)
let rec zip (l1,l2) =
  match (l1,l2) with
    | ([], []) -> []
    | (h1::t1, h2::t2) -> (h1,h2)::(zip(t1,t2))


(* unzip [(1,4); (2,5); (3,6)] = ([1;2;3], [4;5;6]) *)	

(* The following is an iterative version
   - not a typical ML style
   - more like a translation of a C program to ML
   - (but this style can be useful sometimes, as we'll see later *)
	
let rec unzipHelper (zipped, unzipped) =
  match (zipped, unzipped) with
      ((x,y)::rest, (l1,l2)) -> unzipHelper (rest, (l1@[x], l2@[y]))
    | ([], l) -> l

let unzip l = unzipHelper(l, ([],[]))  
  

(* This is the natural recursive algorithm in ML *)
let rec unzip2 l =
  match l with
      [] -> ([], [])
    | (x,y)::rest ->
	let (l1,l2) = (unzip2 rest)
	in (x::l1, y::l2)    

(* datatypes allow users to define their own data structures
   generalization of:
      - enums
      - structs
      - unions
*)

(* a new type with a finite set of values *)

type sign = Positive | Negative | Zero

let signOf n =
  match n with
      0 -> Zero
    | _ when n>0 -> Positive
    | _ -> Negative
	
let signToInt s =
  match s with
      Zero -> 0
    | Negative -> -1
    | Positive -> 1
	
type point =
    Cartesian of (float * float)
  | Polar of (float * float)

let negate p =
  match p with
      Cartesian (x,y) -> Cartesian (-.x, -.y)
    | Polar (rho, theta) -> Polar (rho, theta+.180.0)

let toPolar p =
  match p with
      Polar _ -> p
    | Cartesian (x,y) -> Polar(sqrt(x*.x +. y*.y), atan (y /. x)) 

(* type 'a option = None | Some of 'a *)

let safeDiv (x,y) =
  if y=0 then None else Some (x/y)

(* recursive types *)

type intlist = Nil | Cons of (int * intlist)    

let rec length l =
  match l with
      Nil -> 0
    | Cons(_, rest) -> 1 + (length rest)

(* data only at internal nodes *)	
type binaryTree = Leaf | Node of int * binaryTree * binaryTree	

(* data only at leaves *)  
(* type 'a binTree = Leaf of 'a | Node of 'a binTree * 'a binTree *)
  
let rec preorder tree =
  match tree with
      Leaf -> []
    | Node(n,left,right) -> [n]@(preorder left)@(preorder right)

(* insert into a binary search tree *)
(* < values go to the left subtree
   >= values go the right subtree *)
let rec insert (n,t) = 	
  match t with
      Leaf -> Node(n,Leaf,Leaf)
    | Node(x,t1,t2) ->
	if n<x then Node(x, insert(n,t1), t2)
	else Node(x, t1, insert(n,t2))
	  
(* functions *)

(* functions are *first-class*:
     - they are full-fledged expressions in the language
     - you can pass them to other functions
     - you can return them from other functions
*)

let square x = x*x

let toThe4th x = square(square x)  

let fourthRoot x = sqrt(sqrt x)  

(* twice is a *higher-order* function:
   function that takes another function as an argument *)   
let twice(f,x) = f(f x)

let rec ncalls(f,x,n) =
  match n with
      0 -> x
    | _ ->
	let rest = ncalls(f,x,n-1)
	in f rest

(* anonymous functions
   aka lambdas, closures

   syntax: function pat -> exp
           function x -> x*x

   let f x = x*x is shorthand for
   let f = (function x -> x*x)
*)

	     
