
(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(b1), BoolVal(b2)) when b1=b2 -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()
    | (VarPat(n), v) -> Env.add_binding n v (Env.empty_env())
    | (NilPat, ListVal(NilVal)) -> Env.empty_env()
    | (ConsPat(p1,p2), ListVal(ConsVal(v,lv))) ->
      Env.combine_envs (patMatch p1 v) (patMatch p2 (ListVal lv))
    | _ -> raise MatchFailure

(* Match a value to a list of (pattern, expression) pairs.  Return the
   expression corresponding to the first pattern that matches, as well as
   an environment resulting from the match. Raise a MatchFailure exception
   if no pattern matches.  *)
let rec patMatchAll (l:(mopat * moexpr) list) (value:movalue) : (moexpr * moenv) =
  match l with
      [] -> raise MatchFailure
    | (pat,body)::rest ->
	try
	  let newEnv = patMatch pat value in
	    (body, newEnv)
	with
	    MatchFailure -> patMatchAll rest value


(* Evaluate an expression in the given environment.  Raise a MatchFailure if
   pattern matching fails.  Raise a DynamicTypeError if any other kind of error
   occurs (e.g., trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)	      
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      IntConst(c) -> IntVal(c)
    | BoolConst(b) -> BoolVal(b)
    | Nil -> ListVal(NilVal)
    | Var(s) -> begin
	try Env.lookup s env with
	    Env.NotBound -> raise DynamicTypeError
    end
    | Function(pat,body) -> FunctionVal(None,pat,body,env)
    | BinOp(e1,Cons,e2) ->
	let head = evalExpr e1 env
	and tail = evalExpr e2 env in
	  (match tail with 
	       ListVal lv -> ListVal(ConsVal(head, lv))
	     | _ -> raise DynamicTypeError)
    | BinOp(e1,op,e2) ->
	let arg1 = evalExpr e1 env
	and arg2 = evalExpr e2 env in
	  (match (arg1, arg2) with
	       (IntVal i1, IntVal i2) ->
		 (match op with
		     Plus -> IntVal(i1 + i2)
		   | Minus -> IntVal(i1 - i2)
		   | Times -> IntVal(i1 * i2)
		   | Gt -> BoolVal(i1 > i2)
		   | Eq -> BoolVal(i1 = i2))
	    | _ -> raise DynamicTypeError)
    | Negate(e) ->
      (match evalExpr e env with
	  IntVal v -> IntVal (-v)
	| _ -> raise DynamicTypeError)
    | If(cond,thn,els) ->
	let test = evalExpr cond env in
	  (match test with
	       BoolVal(true) -> (evalExpr thn env)
	     | BoolVal(false) -> (evalExpr els env)
	     | _ ->
		 raise DynamicTypeError)
    | FunctionCall(e1,e2) ->
	let funval = evalExpr e1 env
	and argval = evalExpr e2 env in
	  (match funval with
	      FunctionVal(nameOpt, pat,body,lexicalEnv) ->
		let lenv =
		  match nameOpt with
		      None -> lexicalEnv
		    | Some n -> Env.add_binding n funval lexicalEnv in
		let newEnv = patMatch pat argval in
		evalExpr body (Env.combine_envs lenv newEnv)
	    | _ -> raise DynamicTypeError)
    | Match(matchExp, cases) ->
	let matchVal = evalExpr matchExp env in
	let (body, newEnv) = patMatchAll cases matchVal in
	  evalExpr body (Env.combine_envs env newEnv)
	    

let evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
      | Let(x,e) -> (Some x, evalExpr e env)
    | LetRec(f,p,b) -> (Some f, FunctionVal(Some f, p, b, env))

