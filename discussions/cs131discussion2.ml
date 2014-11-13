(* exception x // the type of x is exn
   
   raise Name of (...maybe some printed parameters?)
   raise exn -> 'a

 *)

(* Array中各个数相乘，若为0 则退出 *)
List.fold_left （fun p x -> if x = 0 then Exit else p * x）1 l

type 'a set = Empty | NonEmpty of 'a List

let add s e
	match s with
	| Empty -> NonEmpty([e])
	| NonEmpty(l) -> if (List.mem e l) then s else NonEmpty(e::l);;

let del s e =
	match s with
	| Empty -> raise NF
	| NonEmpty(l) -> if List.mem e l then let ls = (List.filter(fun x -> x = e) l)

let union s1 s2 =
	(Empty,_) -> s2
   |(_,Empty) -> s1
   |(NonEmpty(l1),NonEmpty(l2)) -> (List.fold_left add s1 l2)
(* |l1 @ (list.filter (fun x -> not(List.mem x l1)) l2) *)

(* Find those elements that in S1 but not in S2 *)
let diff s1 s2 =
	match (s1, s2) with
	(_,Empty) -> s1
   |(Empty,_) -> Empty
   |(NonEmpty(l1),NonEmpty(l2)) -> NonEmpty (List.fold_left(fun p x -> try(del x p) with NotFound -> p) s1 l2)

(* give out all the subset of certain set 
   For example, if the given set is [1;2;3], its subset are[[];[1];[2];[3];[1;2];[2;3];[1;3];[1;2;3]]*)
let rec pSet s =
	match s with
	Empty -> NonEmpty([])
   |NonEmpty(h::t) -> let NonEmpty(pl) = (pSet t) in
   						union (NonEmpty(pl)) (NonEmpty(List.map(fun x -> h::x)pl))

(* 优化 因为用于union的两个集合是完全不重复的 所以可以直接append *)
let NonEmpty(pl) = (pSet t) in NonEmpty(pl@(List.map(fun x -> h:: x)pl))
   