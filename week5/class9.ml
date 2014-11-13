let rec fact n =
	match n with 
		0->1
		|_-> n * (fact(n-1))

(*
	[]      fact 3  ===> 6
	[(n,3)]      	n * fact (2) ===>6
	[(n,2); (n,3)]    			 n*fact(1) ===2
	[(n,1); (n,2); (n,3)]				n* fact(0) ===1
	[n,0); (n,1); (n,2); (n,3)]							===>1

	so linear time and linear space
*
)

(* how would we implement factorial in C?

int fac(int n) {
	int prod = 1;
	int i = n;
	while (i > 0) {
		prod *=i;
		i--;
	}
	return prod;

	linear time / constant space
}

so if implement in ocaml in a certain way (like tail recursive), it can run the code exactly
as in C

* how do we make an iterative version in Ocaml?*
	-do all the work on the way down

	make a helper function
		-has an extra parameter
		-the *accumulating parameter*
		-accumulates the partial results as we recurse
*)
*)

let fact2 n =
	let rec helper i acc=
		match i with
			0 -> acc
			|_-> helper (i-1) (acc*i)
	in helper n 1

(*
	[]      fact2 3  ===> 6
	[(n,3)]      	helper 3 1 ===>6
	[(i,3); (acc,1); (n,3)]    	helper 2 3	 ====> 6
	[(i,2); (acc,3); (n,3)]    	helper 1 6     ====> 6
	[(i,1); (acc,6); (n,3)]    	helper 0 6   ====> 6
	[(i,0); (acc, 6);(n,3)]   		===> 6

	the compiler will apply a tail-call optimization;
	reuse the space for parameters on a tail call;
	the size of the environment never goes up, we just overiding the values. when we get to the bottom, we are done and 
	we dont need to go up any more. so the stack is not growing.
*)

(* 
	a tail call is a function call that is the last operation done in the function body.
	a function is *tail recursive* if all its recursive calls are tail calls

	Ocaml guarantees to implement the tail-call optimization for all tail-recursive functions.
*)

let rec sumList l =
	let rec helper l acc =
		match l with
			[] -> acc
			|x::xs -> helper xs (acc + x)
	in helper l 0

let rec sumList2 l =
		match l with
			[] -> 0
			|x::xs -> x + (sumList2 xs)

let rec diffList l =
	let rec helper l acc =
		match l with
			[] -> acc
			|x::xs -> helper xs (x - acc)
	in helper l 0
(*constant stack space, linear heap space in the size of the list *)

let rec diffList2 l =
		match l with
			[] -> 0
			|x::xs -> x - (diffList2 xs)

let rec diffList3 l =
	let rec helper l acc =
		match l with
			[] -> acc
			|x::xs -> helper xs (x - acc)
	in helper (List.rev l) 0
(*
	now diffList3 is tail recursive
		-but linear space becuase of the reverse
		-constant stack space, but linear heap space ( to hold the list)
*)

(* You can convert any function to be tail recursive
	-it will use a constant amount of stack space
		-but it's not necessarily an asymptotic improvement in space
		-e.g. may need to allocate linear heap space
*)

(* fold_left is tail recursive, fold_right is not tail recursive *)

type tree = Leaf | Node of tree * int * tree
let rec sumTree t =
		match t with
			Leaf -> 0
			|Node (t1,n,t2) -> (SumTree t1) + n + (SumTree t2)

let sumTree2 t =
	let rec helper t acc =
		match t with
			Leaf -> acc
			|Node (t1, n, t2) -> helper t1 (acc + n + (helper t2 0))
	in helper t 0

(*this is not tail recursive ????????*)

let sumTreeTR t =
	let rec helper ts acc =
		match ts with
			[] -> acc
			|Leaf::rest ->helper rest acc
			|(Node (t1, n , t2)) :: rest -> helper (t1::t2::rest) (acc+n) in 
	helper [t] 0

(* helper Node (Node (Leaf, 1, Leaf), 2, Node(Leaf,3,Leaf)) 0)
	helper [Node (Node (Leaf, 1, Leaf), 2, Node(Leaf,3,Leaf))] 0
	helper [Node (Leaf, 1, Leaf); Node(Leaf, 3, Leaf)] 2
	helper [Leaf; Leaf; Node (Leaf,3, Leaf)] 3
	helper [Leaf; Node (Leaf, 3, Leaf)] 3
	helper [Node (Leaf, 3, Leaf)] 3
	helper [Leaf; Leaf] 6
	heler [Leaf] 6
	heler [] 6

	constant stack space
	heap space proportional to the heiht of the tree
*)





















