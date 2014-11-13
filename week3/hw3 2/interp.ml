(* Name: Wen Shi

   UID:504007279

   Others With Whom I Discussed Things: I discussed with Chenxiao Ma and Rui Shao

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
(*what is pattern, is a pattern matches to a value, why not a expression
????????*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
   no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat b1, BoolVal b2) when b1=b2 -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()
    | (VarPat(s), _) -> Env.add_binding s value (Env.empty_env())
    (* not support  match [1] with [1] -> ..
     * must use     match [1] with 1::[] -> ...
     * *)
    | (NilPat, ListVal NilVal) -> Env.empty_env() 
    | (ConsPat(p1, p2), ListVal(ConsVal(v1, ls))) ->
            Env.combine_envs (patMatch p1 v1) (patMatch p2 (ListVal ls))
    | _ -> raise MatchFailure

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)

let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      IntConst(i) -> IntVal(i)
    | BoolConst(b) -> BoolVal(b)
    | Nil -> ListVal NilVal
    | Var(s) -> (try (Env.lookup s env)
                  with Env.NotBound -> raise DynamicTypeError)
    | BinOp(exp1, op, exp2) ->
            let v1=evalExpr exp1 env in
            let v2=evalExpr exp2 env in 
            (match (v1, op, v2) with
                | (IntVal a, Plus, IntVal b) -> IntVal (a+b)
                | (IntVal a, Minus, IntVal b) ->IntVal (a-b)
                | (IntVal a, Times, IntVal b) ->IntVal (a*b)
                | (IntVal a, Eq, IntVal b) -> BoolVal (a=b)
                | (IntVal a, Gt, IntVal b) -> BoolVal (a>b)
                | (_, Cons, ListVal ls) -> ListVal(ConsVal(v1, ls))
                | _ -> raise DynamicTypeError
            )
    | Negate(exp) -> 
             let v1 = evalExpr exp env in
              (match v1 with 
                |IntVal a -> IntVal (-a)
                |_-> raise DynamicTypeError
              )
    | If(exp1, exp2, exp3) -> 
            let v1=evalExpr exp1 env in
            (* different branches may have different return types *)
            (match v1 with 
                | BoolVal(b) -> if b then evalExpr exp2 env else evalExpr exp3 env
                | _ -> raise DynamicTypeError)
    | Function (p1,exp1) -> FunctionVal(None,p1,exp1,env)
    | FunctionCall(exp1, exp2) ->   (*what is exp1??????*)
        let func=evalExpr exp1 env in
        let para=evalExpr exp2 env in
            (match (func, para) with
                  (FunctionVal(None, pat, exp, env1), _) ->
                    let env2 = Env.combine_envs env1 (patMatch pat para) in
                    evalExpr exp env2
                    (*
                    evalExpr exp (Env.add_binding parameter para env1)
                  *)
                | (FunctionVal(Some s, pat, exp, env1), _) ->
                      let env2 = patMatch pat para in
                      let env3 = Env.add_binding s func env2 in
                      let env4 = Env.combine_envs env1 env3 in 
                      evalExpr exp env4
                | _ -> raise DynamicTypeError
            ) 
      
    | Match (exp, lists) -> (
      let v = evalExpr exp env in ( 
        match lists with
        | [] -> raise MatchFailure
        | (pat, e)::rest -> (
          try let env1 = (Env.combine_envs env (patMatch pat v))
          in (evalExpr e env1)
          with MatchFailure -> (evalExpr (Match(exp, rest)) env)
          )
        )
      )


(*
    | Match (l, patlist) -> 
      let rec helper exp plist penv =   (* returned env created by matched pattern and en*)
                (match plist with
                    (pn, en)::rest -> 
                      (try
                        (patMatch pn exp, en)
                      with
                        MatchFailure -> 
                          helper exp rest penv
                      )
                  | [] -> raise MatchFailure
                )
              in 
              let (this_env, this_en) = helper (evalExpr e env) patlist env in
              evalExpr this_en (Env.combine_envs env this_env)
  *)
   
    (*
    |Match(exp, matchlist) ->
      (let rec helper exVal lists envo =
        match lists with 
          [] -> raise MatchFailure
         | (pat, expr)::xs -> 
            try (
              evalExpr expr (Env.combine_envs envo (patMatch pat exVal))
            )
            with
              MatchFailure -> helper exVal xs envo
      ) in helper (evalExpr exp env) matchlist env
   *)
    

(*(ConsPat(p1, p2), ListVal(ConsVal(v1, ls))) ->
            Env.combine_envs (patMatch p1 v1) (patMatch p2 (ListVal ls))
            ？？？？？？？？？？？？？？？？ *)

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)

(*what is expression and what is value???????????????????*)
(* what does Match want to do ?????????????*******)
(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)

let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
    | Let(s, exp) -> (Some s, evalExpr exp env)
    | LetRec(s, pattern, exp) -> 
          let env = Env.add_binding s (FunctionVal(Some s, pattern, exp, env)) env in
          (Some s, FunctionVal(Some s, pattern, exp, env))
    (* |_-> raise DynamicTypeError *)


   
 

