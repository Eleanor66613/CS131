
(* An OCaml Interpreter in OCaml *)

(*

  1. Parsing
    given a string (for example, "let x = 3;;")
    the parser:
    1. check that this represents a syntactically legal program
    2. if it does, then produce a data structure to represent
       this program
      - abstract syntax tree (AST)

  2. Static typechecking
     walk over the AST to check that each expression can be
     given a type

  3. Evaluate the program to a value
     walk over the AST to produce a value

  4. Print the result

  5. Goto Step 1.
  
*)

(*

  A grammar for a small subset of OCaml expressions:
  
  exp ::= boolconst | intconst | var | exp1 && exp2
        | if e1 then e2 else e3 | let x=e1 in e2

*)

(* abstract syntax trees for this language *)
type exp = BoolConst of bool | IntConst of int | Var of string
	   | And of exp * exp | If of exp * exp * exp
	   | Let of string * exp * exp

	       
(* if true && false then 1 else 0 *)
let example =
  If(And(BoolConst true, BoolConst false), IntConst 1, IntConst 0)

(* the legal results of computation *)    
type value = BoolVal of bool | IntVal of int

(* throw this exception whenever there is a type error at runtime
   (trying to invoke an operation with an argument of the wrong type) *)
exception DynamicTypeError  
  
let rec eval (e:exp) (env: (string * value) list) : value =
  match e with
      BoolConst b -> BoolVal b
    | IntConst i -> IntVal i
	(* assume it's not short circuited *)
    | And(e1,e2) ->
	let v1 = eval e1 env in
	let v2 = eval e2 env in
	  (match (v1,v2) with
	       (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
	     | _ -> raise DynamicTypeError)
    | Var x ->
	(* If List.assoc throws the Not_found exception,
	   catch it and throw a dynamic type error *)
	(try 
	   List.assoc x env 
	 with
	     Not_found -> raise DynamicTypeError)
    (* let x = e1 in e2 *)
    | Let (x, e1, e2) ->
	let v1 = eval e1 env in
	  (* add x to the environment for e2's evaluation.
	     this is purely functional, so x automatically
	     gets "popped" off the environment when it goes
	     out of scope! *)
	  eval e2 ((x,v1)::env)
	    
	  
