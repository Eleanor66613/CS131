let rec clone ((e, n) : 'a * int) : 'a list =
  match n with
      0 -> []
    | n -> e::(clone(e, n-1))

let rec rev l =
    match l with
    	  [] -> l
	| x::y -> rev y @ [x]

let fastRev (l : 'a list) : 'a list =
  let rec revHelper(suffixToReverse, reversedPrefix) =
    match suffixToReverse with
	[] -> reversedPrefix
      | x::xs -> revHelper(xs, x::reversedPrefix)
  in
    revHelper(l, [])

let rec tails (l : 'a list) : 'a list list =
  match l with
      [] -> [[]]
    | _::xs -> l::(tails xs)

let rec penultimate (l: 'a list) : 'a option =
    match l with 
	  [x;y] -> Some(x)
	| x::y -> penultimate y  
	| _ -> None

let rec flatten (l: 'a list list) : 'a list =
    match l with
    	  [] -> []
	| x::y -> x @ (flatten y)

let rec intOfDigits l =
    let rec helper l n =
      match l with
       [] -> n
     | x::xs -> helper xs (n*10 + x)
  in helper l 0

let rec merge ((l1,l2) : int list * int list) : int list =
    match (l1, l2) with
      ([], _) -> l2
    | (_, []) -> l1
    | (x::xs, y::ys) when x<y -> x::(merge(xs, l2))
    | (_, y::ys) -> y::(merge(l1, ys))

let rec encode l =
  match l with
    [] -> []
  | [x] -> [(1,x)]
  | x::xs ->
    let (c,v)::rest = encode xs in
      if x=v then (c+1,v)::rest else (1,x)::(c,v)::rest

let rec rotate ((l, n) : 'a list * int) : 'a list = 
  match (l, n) with
     (_, 0)  -> l
  | (x::xs, _) -> rotate(xs@[x], n-1)

let rec dec2bin n =
    match n with 
     0 -> [0]
    | 1 -> [1]
    | _ -> (dec2bin (n/2))@[n mod 2]

let rec pairify (l : 'a list) : ('a * 'a) list =
  match l with
     [] -> []
  | [_] -> []
  | fst::snd::rest -> (fst,snd)::(pairify rest)
