(* Scoping:

   when a variable is used (or referenced), which variable declaration
   does it refer to?

   *lexical* or *static* scoping:  the variable referred to is always
   the declaration of that variable that textually
   nearest in the enclosing scope.

*)

let x = 5
(* x is in scope for the rest of the file *)

let double n = n * 2
(* double in scope for the rest of the file
   n is in scope just in the body of double *)

let x = x+1
in x + 54
(* x is in scope just in the expression after "in" *)
(* the x in x+1 above refers to the earlier definition of x *)

let rec fact n =
  match n with
      0 -> 1
    | i -> i * fact(i-1)
  (* i is in scope just for that case of the pattern match *)
  (* fact is in scope for the rest of the file, and in its own
     definition because of the "rec" *)

     

(* Types

   what is a value?
   - any legal result of a program (expression)

   what is a type?
   - a set of values that have a shared set of operations

   example: int is the set of 32-bit integers
           operations: +, -, *, >, etc.

   int list
   values: [], [1;2;3]
   operations:  ::, @

   int * bool
   values: (1, true)
   operations: access components (by pattern matching)

   int -> bool
   values: (function x -> x>3)
   operations: call the function


   Two dimensions on which to evaluate type systems:

   1. Static vs. dynamic typechecking

   - static means "at compile time"
     - before execution
   - give each expression E in the program a type T
   
     - guarantee: if E has type T then at run time the value of E (if
       E's evaluation terminates normally)will be a member of the set
       T

   advantages:

   - catch errors early
      - checking all possible executions of a function

   - faster at run time
     - no need to re-check types at run time
     - allows some compiler optimizations

   - program documentation

   
   disadvantages:

   - restrictive (conservative)
     - can reject your program even if it's not buggy

   example: heterogenous lists

   # [1; "hi"; 34; "bye"];;
   Error: This expression has type string but an expression was expected of type
          int

   how to achieve the same behavior?
   1. a list of pairs
   # [(1,"hi"); (34, "bye")];;
   - : (int * string) list = [(1, "hi"); (34, "bye")]

   2. use a datatype
   # type intorstring = I of int | S of string;;
   type intorstring = I of int | S of string
   # [I 1; S "hi"; S "bye"; I 34];;
   - : intorstring list = [I 1; S "hi"; S "bye"; I 34]


   - slower compilation


 dynamic typechecking:

    just run the code
    before *every* primitive operation
     - check that the operation is being passed
       values of the right type


 key benefit of static typechecking:  gives a strong guarantee for *all* possible executions of a function
  e.g., consider a function of type int->bool
     - we know that no matter what integer we pass in, we will get back a boolean
       - and will not have any type errors during evaluation
       - (but the execution may not terminate, or may have a run-time exception -- the type system doesn't prevent those things)

   in a dynamically typed language, even if 500 calls to a function did the right thing, there's no guarantee that the
   501st call won't crash with a type error due to a bug, e.g. attempting to invoke + on an int and a string.
   
   
2. Strong vs. weak typechecking
   - strongly typed means the language never allows an operation
     to be invoked with operands of the wrong type
     - never allows undefined behavior

   - weakly typed means you can get into an undefined state
     (and keep executing)

   the only weakly typed languages are C and C++
     - casts
        (int) e
        - the language just assumes that e has type int
     - weakly typed languages are not memory safe
       - a memory safe language is one that only allows
         the program to access memory that it allocated

       violations:

       - access elements of an array out of bounds
       - treat an int as a pointer
       - dangling pointers

   how do strongly typed languages deal with these issues?
      - runtime checks (array bounds)
      - hide the pointers
      - garbage collection


  strong/static: Java, OCaml

  strong/dynamic: Python, PHP, Javascript, Lisp, Smalltalk

  weak/static:  C, C++

  weak/dynamic:  ???

*)
