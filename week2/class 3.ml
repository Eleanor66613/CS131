let square x = x*x
is short for:
let square = (function x -> x*x);;

twice(square, 4)
twice((function x -> x*x),4)

(*funtions can return other functions *)
let returnsAdd () =
	let add(x,y) = x+y
	in add 
val: unit -> int * int -> int = <fun> (*unit means it take no argument *)

let returnsAdd2() =
	function(x,y) -> x+y

let myadd = returnsAdd();;
val myadd : int * int ->int = <fun>

(returnsAdd()) (4,5)
(returnsAdd2()) (4,5)

let twice' f = (function x -> f(f x))

let toThe4thPower = twice' (function x -> x*x)

(twice' (function x -> x*x)) 4;;
-: int = 256
twice' ( function x -> x*x) 4;;
-: int = 256

let add (x,y) = x+y;;
val add: int ( int -> int = <fun>

#let add x y = x + y;;
val add : int -> int -> int = <fun>

#add 3 4;;
-: int = 7

#let addTo3 = add 3;;
-: val adddTo3: int -> int = <fun>

#addTo3 45;;
- : int = 48

# let twice' f = (function x -> f ( f x ))
(* shorthand for: *)
let twice'' = function f -> function x -> f ( f x )
(*equivalent: *)
let twice3 = fun f x -> f ( f x )
(*function will take only one argument and fun can take more then one argument *)
let twice4 f x = f(f x)

(* Use case: iterating over collections *)
let rec incLst l =
	match l with
		[] -> []
		|x::xs -> (x+1) :: (incLst xs)

(*swap the components in a list of pairs *)
let rec swapLst l =
		match l with 
			[] -> []
		 | (x,y) :: rest -> (y,x)::(swap rest)

List.map ;;
-: ('a -> 'b) -> 'a list -> 'b list = < fun>  ??????
List.map (function x -> x +1) [1;2;3;4]
List.map(function(x,y)->(y,x)) [(1,2); (3,4)];;
let incLst l = List.map (function x -> x + 1) l;;
val incLst: int list -> int list = <fun>

let rec map f l =
	match l with 
		[] -> []
		|x:: xs -> (f x) :: (map f xs)
-: ('a -> 'b) -> 'a list -> 'b list = < fun> 

let incByN n l = map (function x -> x + n) l
let incBy3 = incByN 3;

List.filter;;
-: ('a -> bool) -> 'a list -> 'a list  = <fun>
List.filter ( function x -> x>0) [1l -1 2; ;3;-4;0];;
-: int list = [1;2;3]

map-reduce

let rec sumLst l =
	match l with
		[] -> 0
		| x::xs -> x + (sumLst xs)

let rec contains e l =
	match l with
	  [] -> false
	  |x::xs-> when x = e -> true
	  |_-> contains e xs

List.fold_right
- : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>

'a is the type of the given list
'b is the result type of the fold operation

fold_right f [x1,; x2; ...; xn] b =
	f x1 (f x2 (f x3 ... (f xn b)...)))

let sumLst l = List.fold_right (function x y = x + y) [1;2;3;4;5] 0
(*equavalent to *)
let rec sumLst l =
	match l with
		[] -> 0
		| x::xs -> x + (sumLst xs)


let contains2 e l =
	List.fold_right (fun x recursiveResult-> x=e || recursiveResult) l false
let rec contains e l =
	match l with
	  [] -> false
	  |x::xs-> when x = e -> true
	  |_-> contains e xs

let indexOf e l =
	List.fold_right
		(fun x recursiveResult -> if x = e then Some 1 else
			match recursiveResult with
			None -> None
			|Some i -> Some (i+1))
		l None

let rec fold_right f b l=
	match l with
		[] -> b
		|first:: rest -> f first ( fold_right f rest b)


# (+);;
- : int -> int -> int = <fun>

# ( * );;
- : int -> int -> int = <fun>
# (*);;
Warning 1: this is the start of a comment.

*)

List.fold_right ( - ) [1;2;3;4;5] 0
List.fold_right ( * ) [1;2;3;4;5] 1

List.fold_left ( - ) 0 [1;2;3;4;5] 
List.fold_left ( * ) 1 [1;2;3;4;5] 

# List.fold_right;;
- : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>
f x1 (f x2 (f x3 ... (f xn b)...)))
(*
'a is the type of the given list
'b is the result type of the fold operation
*)

# List.fold_left;;
- : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
(f (f (f ... (f b x1)... xn)))

(*
'b is the type of the given list
'a is the result type of the fold operation
*)

let indexOf2 e l =
	List.fold_left
		(fun ( i, res) x ->
			)
			(fun prefixResult x ->
				match prefixResult with) ....


List.fold_right(fun x y -> x :: y) [1;2;3;4;5] []
List.fold_left (fun y x -> x :: y) [] [1;2;3;4;5]

Jquery
(a library of javascript)











































