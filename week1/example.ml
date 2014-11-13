
let rec fact n =
  (* pattern matching *)
  match n with
      0 -> 1
    | _ -> n * (fact(n-1))

(* local variables:
   let x=e1 in e2 *)
let rec isEven n =
  let n = if n<0 then -n else n
  in
    match n with
	0 -> true
      | 1 -> false
      | _ -> isEven(n-2)

let rec isEven2 n =
  match n with
      0 -> true
    | 1 -> false
    | _ -> if n<0 then isEven2(n+2) else isEven2(n-2)

let rec isEven3 n =
  match n with
      0 -> true
    | 1 -> false
    | _ when n<0 -> isEven3(n+2)
    | _ -> isEven3(n-2)
	
let rec sumList l =
  match l with
      [] -> 0
    | first::rest -> first + (sumList rest)
