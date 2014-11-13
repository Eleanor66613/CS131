cs131 discussion

(* Fibonacci *)
let fib n =
	match n with 
	0 -> 0
   |1 -> 1
   |_ -> fib(n-1) + fib(n-2)
(*find all the yinzi for l *)
let find1 l =
	List.map(fun x -> (x,1::(List.fold_left(fun p e -> if x mod e = 0
							then e::(if e = (x/e) then p else (x/e)::p)
							else p)[] (range 2 (sqrt n) 1)))) l
??????

let find2 l =
	List.map(fun x -> (x,(List.filter(fun b -> x mod b = 0)(range 1 (x/2) 1))@[n]))l

??????

(* one way to defind a tree *)
type 'a tree = Node of ('a * 'a tree list)

(* find all the left node of each subtree *)
let leftGet Node(e,tl) =
	match tl with
	[] -> []
    |Node(v,c)::_ -> v::(List.fold_left(fun p x -> p @ (leftGet x))[] tl)


    ????

(* reverse a tree *)
let revTree Node(e,tl) =
	Node(e,List.map(revTree (rev tl)))

(* fold_left (f : 'a -> 'b -> 'a) //the former two is input
             (i : 'a)
             (l : 'b list)
             :'a 
 *)

?????

(* Reverse an array *)
let rev l =
  let rec rev1 l temp =
  match l with
  [] -> temp
  |h::t -> (rev1 t (h::temp))   
  in rev1 l [];;

let rev l =
  match l with
    [] -> []
    |h :: t -> (rev t) @ h
    