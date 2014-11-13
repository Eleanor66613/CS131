
let rec fact n =
  match n with
      0 -> 1
    | _ -> n * (fact(n-1))

(*
  []               fact 3  ==> 6
  [(n,3)]             n * fact(2) ==> 6
  [(n,2);(n,3)]            n * fact(1)  ==> 2
  [(n,1);(n,2);(n,3)]             n * fact(0) ==> 1
  [(n,0);(n,1);(n,2);(n,3)]             ==> 1

  linear time / linear space

  How would we implement factorial in C?

  int fact(int n) {
    int prod = 1;
    int i = n;
    while(i > 0) {
      prod *= i;
      i--;
    }
    return prod;
  }

  linear time / constant space
*)

(* How do we make an iterative version in OCaml?
    - do all the work on the way down

   Make a helper function
    - has an extra parameter
    - the *accumulating parameter*
    - accumulates the partial results as we recurse
*)

let fact2 n =
  let rec helper i acc =
    match i with
	0 -> acc
      | _ -> helper (i-1) (acc * i)
  in helper n 1
	
(*
  []                               fact2 3 ==> 6
  [(n,3)]                          helper 3 1 ==> 6 
  [(i,3);(acc,1);(n,3)]            helper 2 3 ==> 6
  [(i,2);(acc,3);(i,3);(acc,1);(n,3)]
                                   helper 1 6  ==> 6
  [(i,1);(acc,6);(i,2);(acc,3);(i,3);(acc,1);(n,3)]
                                   helper 0 6 ==> 6
  [(i,0);(acc,6);(i,1);(acc,6);(i,2);(acc,3);(i,3);(acc,1);(n,3)]
                                   ==> 6

  the compiler will apply a tail-call optimization:
    reuse the space for parameters on a tail call

  []                               fact2 3 ==> 6
  [(n,3)]                          helper 3 1 ==> 6 
  [(i,3);(acc,1);(n,3)]            helper 2 3 ==> 6
  [(i,2);(acc,3);(n,3)]
                                   helper 1 6  ==> 6
  [(i,1);(acc,6);(n,3)]
                                   helper 0 6 ==> 6
  [(i,0);(acc,6);(n,3)]
                                   ==> 6
  
*) 
  
(* A tail call is a function call that is the last operation done
   in a function body.

   A function is *tail recursive* if all its recursive calls are
   tail calls.

   OCaml guarantees to implement the tail-call optimization for
   all tail-recursive functions.
*)

let rec sumList l =
  match l with
      [] -> 0
    | x::xs -> x + (sumList xs)

let sumList2 l =
  let rec helper l acc =
    match l with
	[] -> acc
      | x::xs -> helper xs (x+acc)
  in helper l 0
(* constant stack space *)

(* [a;b;c;d]
   a - (b - (c - d)) *)
let rec diffList l =
  match l with
      [] -> 0
    | x::xs -> x - (diffList xs)

let diffList2 l =
  let rec helper l acc =
    match l with
	[] -> acc
      | x::xs -> helper xs (x - acc)
  in helper (List.rev l) 0

(* now diffList2 is tail recursive
   - constant stack space
   - but linear heap space (to reverse the list)
*)
       
(* You can convert any function to be tail recursive
    - it will use a constant amount of stack space
    - but it's not necessarily an asymptotic improvement
      in space
      - e.g., may need to allocate more heap space
*)

type tree = Leaf | Node of tree * int * tree

let rec sumTree t =
  match t with
      Leaf -> 0
    | Node(t1,n,t2) -> (sumTree t1) + n + (sumTree t2)

(* NOT tail recursive *)	
let sumTree2 t =
  let rec helper t acc =
    match t with
	Leaf -> acc
      | Node(t1,n,t2) -> helper t1 (acc + n + (helper t2 0))
  in helper t 0
       
let sumTreeTR t =
  let rec helper ts acc =
    match ts with
	[] -> acc
      | Leaf::rest -> helper rest acc
      | (Node(t1,n,t2))::rest -> helper (t1::t2::rest) (n+acc)
  in helper [t] 0

(* helper [Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))] 0
   helper [Node(Leaf, 1, Leaf); Node(Leaf, 3, Leaf)] 2
   helper [Leaf; Leaf; Node(Leaf, 3, Leaf)] 3
   helper [Leaf; Node(Leaf, 3, Leaf)] 3
   helper [Node(Leaf, 3, Leaf)] 3
   helper [Leaf; Leaf] 6
   helper [Leaf] 6
   helper [] 6

  constant stack space
  heap space proportional to the height of the tree
*)   
