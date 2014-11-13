
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result, both expressed as strings.
   use the string "dynamic type error" if a DynamicTypeError is expected to be raised.
   use the string "match failure" if a MatchFailure is expected to be raised.
   use the string "implement me" if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests = [
    ("3", "3");                                   (* IntConst of int *)   
    ("true", "true");                               (* BoolConst of bool *)          
    ("false", "false");                             (* BoolConst of bool *)
    ("[]", "[]");                                   (* Nil *)
    ("let x = 34", "val x = 34");                   (* Let of string * moexpr *)
    ("x", "34");                                    (* Var of string *)
    ("y", "dynamic type error");                    (* Var of string *)
    ("3 + 4", "7");                                 (* BinOp of moexpr * Plus * moexpr *)
    ("x + 4", "38");                                (* BinOp of moexpr * Plus * moexpr *)
    ("3 * 4", "12");                                (* BinOp of moexpr * Minus * moexpr *)
    ("3 + 4", "7");                                 (* BinOp of moexpr * Times * moexpr *)
    ("3 = 4", "false");                             (* BinOp of moexpr * Eq * moexpr *)
    ("5 = 5", "true");                              (* BinOp of moexpr * Eq * moexpr *)
    ("3 > 4", "false");                             (* BinOp of moexpr * Gt * moexpr *)
    ("4 > 3", "true");                              (* BinOp of moexpr * Gt * moexpr *)
    ("4::[]", "[4]");                               (* BinOp of moexpr * Cons * moexpr *)
    ("4::[5]", "[4; 5]");                           (* BinOp of moexpr * Cons * moexpr *)
    ("4::[5;6]", "[4; 5; 6]");                      (* BinOp of moexpr * Cons * moexpr *)
    ("-x", "-34");                                  (* Negate of moexpr *)
    ("-8", "-8");                                   (* Negate of moexpr *)
    ("-(8+9)", "-17");                              (* Negate of moexpr *)
    ("let y = 50", "val y = 50");                   (* Let of string * moexpr *)
    ("if x > y then true else false", "false");     (* If of moexpr * moexpr * moexpr *)
    ("if y > x then true else false", "true");       (* If of moexpr * moexpr * moexpr *)

    (* My test cases, check if correct *)
    (* simple tests *)
    ("x + 4", "38");
    ("x - 4", "30");
    ("x * 4", "136");
    ("x = 34", "true");
    ("x = 0", "false");
    ("x > 33", "true");
    ("35 > x", "true");
    ("x > 35", "false");
    ("5::3", "dynamic type error");
    ("-x", "-34");
    ("33 > -x", "true");
    ("-[]", "dynamic type error");
    ("(if x = 34 then 2 else 3)::[2; 3]", "[2; 2; 3]");
    ("if 1 then 2 else 3", "dynamic type error");
    ("if [2; 3] then 5 else 10", "dynamic type error");
    ("if 2 > 5 then 20 else 3", "3");

    (* non-recursive function testing *)
    ("let newfunc = function x -> x * x", "val newfunc = <fun>");
    ("newfunc 2", "4");
    ("newfunc(2)", "4");
    ("let timesThreeNums = function a -> function b -> function c -> a * b * c", "val timesThreeNums = <fun>");
    ("timesThreeNums 2 3 4", "24");
    ("let multSix = timesThreeNums 2 3", "val multSix = <fun>");
    ("multSix 2", "12");
    ("let a = function true -> false", "val a = <fun>");
    ("a 1", "match failure"); (* not sure about this one *)
    ("let b = function _ -> true", "val b = <fun>");
    ("b 1", "true");
    ("b [1; 2; 3]", "true");
    ("let decapitate = function head::tail -> tail", "val decapitate = <fun>");
    ("decapitate [1; 2; 3]", "[2; 3]");
    ("decapitate [1]", "[]");
    ("decapitate []", "match failure");
    ("(function _ -> x) 42", "34");

    (* pattern matching *)
    ("let l = [1; 2; 3]", "val l = [1; 2; 3]");
    ("match l with 1::2::[] -> true | _ -> false", "false");
    ("match l with 1::2::rest -> rest", "[3]");
    ("match 5 with s -> s", "5");
    ("match 10 with _ -> x", "34");
    ("(function s -> match s with head::tail -> head = x) [34; 2]", "true");
    ("match 20 with x -> x", "20");
    ("match 1 with 1 -> (match 2 with 1 -> false) | _ -> true", "match failure");

    (* recursive function testing *)
    ("let rec recTest num = match num with 0 -> 0 | s -> 1 + (if s > 0 then recTest(s-1) else recTest(s+1))", "val recTest = <fun>");
    ("recTest 10", "10");
    ("recTest (-10)", "10");
    ("let rec recTest2 _ = recTest2", "val recTest2 = <fun>");
    ("recTest2", "<fun>");
    ("recTest2 1 2 3 4 5 6 7 8 9 10 11 12", "<fun>");
    ("let rec fact n = if n > 0 then n * fact (n - 1) else 1", "val fact = <fun>");
    ("fact 5", "120");
    ("let rec map f = function l -> match l with [] -> [] | x::xs -> (f x)::(map f xs)", "val map = <fun>");
    ("(map fact) [1;2;3;4;5]", "[1; 2; 6; 24; 120]");

    (* Scope testing *)
    ("let f = function x -> x", "val f = <fun>");
    ("let g = function x -> x + (f x)", "val g = <fun>");
    ("let f = function x -> x * 23", "val f = <fun>");
    ("g 5", "10");
    ("f 5", "115");
    ("let rec f l = function v -> match l with [] -> v | x::xs -> (f xs v+1)", "val f = <fun>");
    ("f [5;3;2] 0", "3");
    ("let rec f g = match g with f -> f 0", "val f = <fun>");
    ("f (function _ -> 5)", "5"); (* recursive name shadowed by match. *)
    ("let rec f l = match l with [] -> 0 | x::xs -> x + f xs", "val f = <fun>");
    ("f [1;2;3]", "6"); (* Recursive name shadowing other f *)

  (* Testing Errors *)
  ("false + false", "dynamic type error");
  ("1 + false", "dynamic type error");
  ("1 - false", "dynamic type error");
  ("1 * false", "dynamic type error");
  ("1 > false", "dynamic type error");
  ("1 = false", "dynamic type error");
  ("4::5", "dynamic type error");

    (* 
    Tricky with env variable, not sure if our mocaml need to support this.
    It relate to the sequence that add_binding 'function f' first or add_binding 'variable f' first
    *)
    ("let f = 2", "val f = 2");
    ("let rec f f = match f with 0 -> f | _ -> (f+1)", "val f = <fun>");
    ("f 1","2");
    ]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  
