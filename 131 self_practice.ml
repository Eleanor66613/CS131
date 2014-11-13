(* Problem 1b
   unzip : ('a * 'b) list -> 'a list * 'b list *)

let rec unzip l = list.fold_right(fun (x,y)(l1,l2) -> (x::l1, y::l2)) l ([], [])

(* Problem 1c
   encode : 'a list -> (int * 'a) list *)
let encode l =
	list.fold_right(fun x ls -> 
		match ls with
		[] -> (1, x)
		|(num, char) :: xs -> if x = char then (num+1, char)::xs else (1,x) :: ls
	) l []
(* Problem 1d
   intOfDigits : int list -> int *)
let intOfDigits l =
	list.fold_left (fun sofar x -> sofar*10 + x ) 0 l

(* Problem 2a
   map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)


(* Problem 2b
   zip : 'a list * 'b list -> ('a * 'b) list *)


(* Problem 2c
   foldn : (int -> 'a -> 'a) -> int -> 'a -> 'a *)


(* Problem 2d
   clone : 'a * int -> 'a list *)



(* Problem 3a
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b option
*)  

   

(* Problem 3b
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b option
*)  
    

	
(* Problem 3c
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b option
*)  
