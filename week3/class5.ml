(*key point: all of this type instantiation happens at compile time *)

let swap (x,y) = (y,x)
(*swp: ('a * 'b) -> ('b * 'a) 
swap (3, "hi")
1. typecheck (3, "hi")
(int * string)
2. is (int*string) a special case of ('a * 'b) ?
yes, where 'a maps to int and 'b maps to string

3. result type is (string * int)

*)

(* Parametric polymorphism:
	-one piece of code (a function)
	-can pass it arguments of many different types
*)

(* contrast with overloading:
	-many functions
		-each taking a different type of argument
	-functions have the same name
*)

(* but ocaml also have overloading)

let rec contains e l =
	match l with
		[] -> false
		| x::xs -> x = e || (contains e xs)
here "="  in "x = e" is overloading, not polymorphism??????
= for string, list, int.. are all different
for convenience, you can treat them as polymorphism, but actually it is not.
For example, if x is a function, it get errors.

type ('a ,'b) dict2 = Empty | Entry of 'a * 'b * ('a, 'b) dict2

is there any difference bewtween c++'s template and the polymorphism?
Yes. ocaml checks the body of the function just once and once the typechecking past, it will no check any more. ???
it will never check the body of the function. But it will check arguments also for everytime you run it.

In template, you need to check it very single time when you use it.

hw3:
An Ocaml interpreter in Ocaml
1. parsing
	-given a string ( for example, "let x = 3;;")
	the parser:
		1. check that this represents a syntactically legal program
		2. if it does, then produce a data structure to represent this program
		-abstract tyntax tree [AST]
2. static typechecking
	walk over the AST to check that each expression can be given a type

3. evaluate the program to a value
	walk over the AST to product a value

4. print the result

5. Goto step 1
*)

(*
	exp ::= boolconst | intconst | var | exp1 && exp2 | if e1 then e2 else e3 | let x = e1 in e2
*)
(*abstract syntax tress for this language *)
type exp = BoolConst of bool | IntConst of int | Var of string | And of exp * exp | If of
	exp*exp*exp | Let of string * exp * exp

(* if true && false then 1 else 0 *)

let example = If(And(BoolConst true, BoolConst false), InConst 1, IntConst 0)

(* the legal results of computation *)

type value = BoolVal of bool | IntVal of int

(*exp and value are completely different things, exp is expression value is value, so
they need different names to represent (BoolConst vs. BoolVal) *)

let rec eval (e: exp) : value =
	match e with
		BoolConst b -> BoolVal b
		| IntConst i -> IntVal i
		|And (e1, e2) -> 
			let v1 = eval e1 in
				match v1 with
					BoolVal false -> BoolValue false
					|BoolVal true -> eval e2
		|And (e1, e2) ->
			let v1 = eval e1 in
			let v2 = eval e2 in
				match (v1,v2) with
					(BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
				|_-> raise DynamicTypeError)








































