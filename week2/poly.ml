(* Parametric Polymorphism *)

(*

  static typechecking catches certain errors early
   - type errors
   - invoking a primitive operation with operands of the wrong type

  issue: static typechecking is always conservative
    - will reject some programs that don't signal type errors
    - [1;"hi"]
    - if false then 0 else true
  
*)
       
(* Parametric polymorphism deals with one limitation
   of static typechecking
    - allows more programs to typecheck while still
      guaranteeing no type errors at run time *)

let rec length l =
  match l with
      [] -> 0
    | _::xs -> 1 + (length xs)

(* length: 'a list -> int

   'a is a *type variable*

   can think of 'a as an extra to the parameter to the function

   length <int> [1;2;3;4;5]

   length <bool> [true;false;false]

*)

(* at compile time, the static typechecker instantiates 'a
   at each call site.

   length : 'a list -> int
   length [1;2;3;4]
   how do we instantiate 'a?
   1. typecheck argument
      [1;2;3;4] : int list

   2. check whether that type is a special case of
      the argument type of length
      is (int list) a special case of ('a list)?
      i.e., can I find an instantiation of 'a that
       makes these types equal?
      - map 'a to int

*)

(* key point: all of this type instantiation happens at compile time
*)	

let swap (x,y) = (y,x)

(* swap: ('a * 'b) -> ('b * 'a)

   swap (3, "hi")

   1. typecheck (3,"hi")
      (int * string)

   2. is (int * string) a special case of ('a * 'b)?
      yes, where 'a maps to int and 'b maps to string

   3. so the result type is (string * int) 

*)

(* Parametric polymorphism:
     - one piece of code (a function)
     - can pass it arguments of many different types
*)

(* Contrast with overloading:
     - many functions
        - each taking a different type of argument
     - functions have the same name
*)

  
(* OCaml actually does have a few overloaded functions, such as =.
   But it cheats and treats them as if they are polymorphic!
   This is convenient (e.g., can write a single contains function,
   as below), but it also means we can get a type error at run time
   if we try to use equality with types that don't support it, such
   as function types. *)
let rec contains e l =
  match l with
      [] -> false
    | x::xs -> x=e || (contains e xs)
  
  
