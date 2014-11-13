
(* Name: Wen Shi

   UID: 504007279

   Others With Whom I Discussed Things: 

   Other Resources I Consulted: 
   
*)

(* Problem 1a
   doubleAllPos : int list -> int list *)

let doubleAllPos l =
   List.map(fun x -> if x > 0 then (x * 2) else x) l;;

(* Problem 1b
   unzip : ('a * 'b) list -> 'a list * 'b list *)

let unzip l =
   List.fold_right(fun (x,y) (l1, l2) -> (x::l1, y::l2)) l ([],[]);;

(* Problem 1c
   encode : 'a list -> (int * 'a) list *)
let encode l =
   List.fold_right(
      fun current rest-> 
         match rest with
            [] -> (1, current) :: [] 
            |(number, current2) :: rest2 -> 
               if current = current2 then (number+1, current2) :: rest2
               else (1, current) :: rest
      )
   l [];;

(* Problem 1d
   intOfDigits : int list -> int *)

let intOfDigits l=
   List.fold_left 
      (fun x y -> x*10+y)
   0 l;;

(* Problem 2a
   map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
let rec map2 f l1 l2 =
   match l1 with
      [] -> []
      |h1::rest1 -> match l2 with
         h2::rest2-> (f h1 h2) :: map2 f rest1 rest2;;

(* Problem 2b
   zip : 'a list * 'b list -> ('a * 'b) list *)
let zip (l1, l2) =
   map2 (fun x y -> (x,y)) l1 l2;;

(* Problem 2c
   foldn : (int -> 'a -> 'a) -> int -> 'a -> 'a *)
let rec foldn f n b =
   match n with
      1 -> f 1 b
      |_-> f n (foldn f (n-1) b);;

(* Problem 2d
   clone : 'a * int -> 'a list *)

let clone (e,n) =
   foldn (fun x y -> e::y) n [];;

(* Problem 3a
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b option
*)  
type 'a option = None | Some of 'a;;

let empty1 () = [];;

let put1 key value dict =
   (key,value)::dict;;

let rec get1 key dict=
   match dict with
      [] -> None
      |(key2, value2) :: rest -> if key2 = key then Some value2 else get1 key rest;;
   

(* Problem 3b
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b option
*)  
    
type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a,'b) dict2;;
let empty2() = Empty;;
let put2 key value dict =
   Entry (key,value, dict);;
let rec get2 key dict =
   match dict with
      Empty -> None
      |Entry (a,b, restDict) -> if a = key then Some b else get2 key restDict;;
	
(* Problem 3c
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b option
*)  

type ('a,'b) dict3 = ('a -> 'b option);;

let empty3() =
   fun _ -> None;;
   
let put3 key value dict =
   fun x -> if x = key then (Some value) else dict x;;

let get3 key dict =
   dict key;;


