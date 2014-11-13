

(* Problem 1a *)

let doubleAllPos l = List.map (function x -> if x>0 then 2*x else x) l
	
(* Problem 1b *)

let unzip l =
  List.fold_right
    (fun (x,y) (l1,l2) -> (x::l1, y::l2)) l ([],[])

(* Problem 1c *)

let encode l =
  List.fold_right
    (fun x res ->
      match res with
	  [] -> [(1,x)]
	| (n,y)::rest -> if x=y then (n+1,y)::rest else (1,x)::(n,y)::rest) l []
  
(* Problem 1d *)

let intOfDigits l =
  List.fold_left
    (fun n d -> 10*n + d) 0 l

(* Problem 2a *)

let rec map2 f l1 l2 =
  match (l1,l2) with
      ([],[]) -> []
    | (x::xs,y::ys) -> (f x y)::(map2 f xs ys)

(* Problem 2b *)

let zip (l1,l2) = map2 (fun x y -> (x,y)) l1 l2

(* Problem 2c *)

let rec foldn f n b =
  match n with
      0 -> b
    | _ -> f n (foldn f (n-1) b)

(* Problem 2d *)

let clone(e,n) = foldn (fun _ l -> e::l) n []      

  
(* Problem 3a
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b option
*)  

let empty1() = []
      
let put1 k v d = (k,v)::d

let rec get1 k d =
  match d with
      [] -> None
    | (k',v')::d' -> if k=k' then Some v' else get1 k d'

	
(* Problem 2b
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b option
*)  
    
type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a,'b) dict2

let empty2() = Empty
  
let put2 k v d = Entry (k,v,d)

let rec get2 k d =
  match d with
      Empty -> None
    | Entry (k', v', d') ->
      if k'=k then Some v' else (get2 k d')

	
(* Problem 2c
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b option
*)  

type ('a,'b) dict3 = ('a -> 'b option)

let empty3() = (function s -> None)    
    
let put3 k v d =
  (fun k' -> if k=k' then Some v else (d k'))

let get3 k d = d k

  
