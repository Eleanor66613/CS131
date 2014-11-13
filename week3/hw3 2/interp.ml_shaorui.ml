(* Name: Rui Shao

   UID: 704422314

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

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
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i = j -> Env.empty_env()
    | (BoolPat(i), BoolVal(j)) when i = j-> Env.empty_env()
    | (WildcardPat,_) -> Env.empty_env()
    | (VarPat(s),svalue) -> (Env.add_binding s svalue (Env.empty_env()))
    | (NilPat,ListVal(NilVal)) -> Env.empty_env()
    | (ConsPat(p1, p2), ListVal(ConsVal(v1, v2))) -> (Env.combine_envs (patMatch p1 v1) (patMatch p2 (ListVal(v2))))   
    | _ -> raise MatchFailure

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
      IntConst(i) -> IntVal(i)
    | BoolConst(i) -> BoolVal(i)
    | Nil -> ListVal(NilVal)
    | Var(s) -> (try (Env.lookup s env) with Env.NotBound -> raise DynamicTypeError)
    | BinOp(e1,o,e2) -> 
      ( match ((evalExpr e1 env),o,(evalExpr e2 env)) with
          (IntVal(i),Plus,IntVal(j)) -> IntVal(i+j) 
          |(IntVal(i),Minus,IntVal(j)) -> IntVal(i-j)
          |(IntVal(i),Times,IntVal(j)) -> IntVal(i*j)
          |(IntVal(i),Eq,IntVal(j)) -> BoolVal(i=j)
          |(IntVal(i),Gt,IntVal(j)) -> BoolVal(i>j)
          |(x,Cons,ListVal(list)) -> ListVal(ConsVal(x,list))
          |_ -> raise DynamicTypeError
      )

    | Negate(e) ->
      ( match  (evalExpr e env) with
          (IntVal(i)) -> IntVal(-i)
          |_ -> raise DynamicTypeError
      )
    | If(e1,e2,e3) ->
      ( match (evalExpr e1 env) with
          (BoolVal(bval)) -> if bval then (evalExpr e2 env) else (evalExpr e3 env)
          |_ -> raise DynamicTypeError
      )
    | Function(p,e) -> (FunctionVal(None,p,e,env))
    | FunctionCall(e1,e2) -> (let func = (evalExpr e1 env) in
                              let para = (evalExpr e2 env) in
                              match func with
                              FunctionVal(name,p,e,funenv) -> (match name with
                                                                Some(x) -> (let newenv = try (patMatch p para) with MatchFailure -> raise MatchFailure in
                                                                            let bindenv = (Env.add_binding x func newenv) in
                                                                            let combineenv = (Env.combine_envs funenv bindenv)in (evalExpr e combineenv)                       
                                                                           )
                                                               |None -> ( let newenv = try (patMatch p para) with MatchFailure -> raise MatchFailure in
                                                                          let combineenv = (Env.combine_envs funenv newenv) in (evalExpr e combineenv) 
                                                                        )
                                                             )
                              |_ -> raise DynamicTypeError
                             )

    | Match(e,l) -> match l with
                          [] -> raise MatchFailure
                          |(x,y)::rest -> (try
                                              let newval = try (evalExpr e env) with MatchFailure -> raise MatchFailure in
                                              let newenv = try (patMatch x newval) with MatchFailure -> raise MatchFailure in
                                              let combineenv = (Env.combine_envs env newenv)
                                              in (evalExpr y combineenv)
                                                                                
                                           with MatchFailure -> (evalExpr (Match(e, rest)) env)
                                          )   

                      

    | _ -> raise DynamicTypeError

(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
    | Let(s,e) -> (Some s, evalExpr e env)
    | LetRec(s,p,e) -> (Some (s),FunctionVal(Some(s),p,e,env))
    | _ -> raise (ImplementMe "let and let rec not implemented")

