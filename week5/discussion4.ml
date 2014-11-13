(* Tail Recursive *)
let rec f x = if x = 0 then 1 else (f((f(x-1))-1)

tree = Leaf | Node of tree*int*tree
let sumTree t 
	= let sumT l a 
		= match l with
			[] -> a
			|Leaf::x -> (sumT x a)
			|Node(t1,n,t2)::x-> sumT (t1::t2::x) (a+n)
	in summT [t] 0
(*The above is tail recursive*)

(*in Ocaml, all are in heap, but only funcions are in stack *)
(*hea space is for run time and stack spae is for compile time *)
(* if w ehave a tree with a height of n,  the heap space we need is maximum n+1,
the time complexity of 2 to (h+1) since (2 to n) + (2 to (n-1)) + (2 to (n-2)...))


