type token =
  | INT of (int)
  | BOOL of (bool)
  | VAR of (string)
  | FUNCTION
  | PIPE
  | WILDCARD
  | FN_ARROW
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | EQ
  | IN
  | MATCH
  | WITH
  | LBRACK
  | RBRACK
  | CONS
  | SEMICOLON
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | GT
  | COLON
  | EOD

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
exception ConstTypeParseError

let print_productions = false
let print p = if print_productions then print_string (p^"\n") else ()
# 39 "parser.ml"
let yytransl_const = [|
  260 (* FUNCTION *);
  261 (* PIPE *);
  262 (* WILDCARD *);
  263 (* FN_ARROW *);
  264 (* IF *);
  265 (* THEN *);
  266 (* ELSE *);
  267 (* LET *);
  268 (* REC *);
  269 (* EQ *);
  270 (* IN *);
  271 (* MATCH *);
  272 (* WITH *);
  273 (* LBRACK *);
  274 (* RBRACK *);
  275 (* CONS *);
  276 (* SEMICOLON *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* PLUS *);
  280 (* MINUS *);
  281 (* TIMES *);
  282 (* GT *);
  283 (* COLON *);
  284 (* EOD *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\005\000\005\000\005\000\007\000\007\000\008\000\008\000\008\000\
\009\000\009\000\010\000\010\000\011\000\011\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\014\000\014\000\006\000\
\006\000\015\000\015\000\004\000\004\000\016\000\016\000\016\000\
\016\000\000\000"

let yylen = "\002\000\
\002\000\001\000\004\000\006\000\001\000\004\000\004\000\006\000\
\001\000\003\000\003\000\001\000\003\000\001\000\003\000\003\000\
\001\000\003\000\001\000\002\000\001\000\002\000\001\000\001\000\
\003\000\003\000\001\000\001\000\002\000\001\000\003\000\003\000\
\004\000\004\000\005\000\001\000\003\000\001\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\027\000\028\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\000\000\000\002\000\000\000\
\009\000\000\000\000\000\017\000\000\000\021\000\023\000\040\000\
\039\000\000\000\000\000\000\000\038\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\026\000\010\000\011\000\013\000\000\000\000\000\000\000\041\000\
\006\000\037\000\000\000\003\000\000\000\000\000\007\000\031\000\
\000\000\000\000\000\000\008\000\004\000\000\000\000\000\033\000\
\000\000\000\000\000\000\035\000"

let yydgoto = "\002\000\
\013\000\014\000\036\000\028\000\016\000\071\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\037\000\080\000\030\000"

let yysindex = "\006\000\
\204\255\000\000\000\000\000\000\000\000\015\255\017\000\002\255\
\017\000\228\255\017\000\021\255\000\000\232\254\000\000\020\255\
\000\000\080\255\246\254\000\000\021\255\000\000\000\000\000\000\
\000\000\026\255\015\255\038\255\000\000\028\255\040\255\039\255\
\052\255\046\255\000\000\043\255\048\255\042\255\021\255\000\000\
\010\255\010\255\010\255\010\255\010\255\021\255\000\000\054\255\
\017\000\015\255\017\000\017\000\015\255\015\255\017\000\000\000\
\000\000\000\000\000\000\000\000\246\254\246\254\021\255\000\000\
\000\000\000\000\058\255\000\000\057\255\071\255\000\000\000\000\
\017\000\017\000\017\000\000\000\000\000\079\255\015\255\000\000\
\078\255\017\000\079\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\245\255\136\255\000\000\070\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\255\000\000\000\000\
\000\000\000\000\000\000\069\255\000\000\000\000\092\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\156\255\176\255\114\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\255\000\000\000\000\
\000\000\000\000\250\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\231\255\000\000\000\000\254\255\000\000\
\016\000\000\000\247\255\236\255\003\000\036\000\023\000\000\000"

let yytablesize = 310
let yytable = "\015\000\
\047\000\048\000\039\000\040\000\032\000\031\000\001\000\034\000\
\029\000\038\000\003\000\004\000\005\000\033\000\046\000\003\000\
\004\000\024\000\047\000\036\000\025\000\003\000\004\000\005\000\
\066\000\036\000\010\000\069\000\070\000\029\000\011\000\026\000\
\041\000\012\000\036\000\027\000\063\000\010\000\058\000\059\000\
\060\000\011\000\047\000\035\000\049\000\042\000\050\000\065\000\
\051\000\067\000\068\000\052\000\029\000\081\000\053\000\029\000\
\029\000\032\000\032\000\061\000\062\000\054\000\055\000\057\000\
\032\000\056\000\032\000\073\000\032\000\074\000\032\000\076\000\
\077\000\078\000\019\000\064\000\032\000\075\000\019\000\019\000\
\083\000\029\000\019\000\079\000\082\000\019\000\030\000\019\000\
\019\000\019\000\072\000\019\000\019\000\019\000\019\000\019\000\
\020\000\019\000\043\000\000\000\020\000\020\000\044\000\045\000\
\020\000\084\000\000\000\020\000\000\000\020\000\020\000\020\000\
\000\000\020\000\020\000\020\000\020\000\020\000\018\000\020\000\
\000\000\000\000\018\000\018\000\000\000\000\000\018\000\000\000\
\000\000\018\000\000\000\018\000\018\000\018\000\000\000\018\000\
\018\000\018\000\018\000\018\000\014\000\018\000\000\000\000\000\
\014\000\014\000\000\000\000\000\014\000\000\000\000\000\014\000\
\000\000\014\000\014\000\014\000\000\000\014\000\014\000\014\000\
\015\000\014\000\000\000\014\000\015\000\015\000\000\000\000\000\
\015\000\000\000\000\000\015\000\000\000\015\000\015\000\015\000\
\000\000\015\000\015\000\015\000\016\000\015\000\000\000\015\000\
\016\000\016\000\000\000\000\000\016\000\000\000\000\000\016\000\
\000\000\016\000\016\000\016\000\000\000\016\000\016\000\016\000\
\000\000\016\000\000\000\016\000\003\000\004\000\005\000\006\000\
\000\000\000\000\000\000\007\000\000\000\000\000\008\000\000\000\
\000\000\000\000\009\000\000\000\010\000\000\000\000\000\000\000\
\011\000\000\000\000\000\012\000\003\000\004\000\005\000\006\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\000\000\010\000\035\000\000\000\000\000\
\011\000\012\000\000\000\012\000\000\000\012\000\012\000\000\000\
\000\000\012\000\034\000\034\000\012\000\000\000\012\000\000\000\
\012\000\034\000\012\000\034\000\000\000\034\000\012\000\034\000\
\012\000\003\000\004\000\005\000\006\000\034\000\000\000\000\000\
\007\000\000\000\000\000\000\000\000\000\000\000\005\000\009\000\
\000\000\010\000\005\000\005\000\000\000\011\000\000\000\000\000\
\012\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\005\000"

let yycheck = "\001\000\
\021\000\027\000\012\000\028\001\003\001\007\000\001\000\009\000\
\006\000\011\000\001\001\002\001\003\001\012\001\025\001\001\001\
\002\001\003\001\039\000\007\001\006\001\001\001\002\001\003\001\
\050\000\013\001\017\001\053\000\054\000\027\000\021\001\017\001\
\013\001\024\001\022\001\021\001\046\000\017\001\041\000\042\000\
\043\000\021\001\063\000\018\001\007\001\026\001\019\001\049\000\
\009\001\051\000\052\000\013\001\050\000\079\000\003\001\053\000\
\054\000\009\001\010\001\044\000\045\000\016\001\020\001\022\001\
\016\001\018\001\018\001\010\001\020\001\013\001\022\001\073\000\
\074\000\075\000\005\001\022\001\028\001\007\001\009\001\010\001\
\082\000\079\000\013\001\005\001\007\001\016\001\018\001\018\001\
\019\001\020\001\055\000\022\001\023\001\024\001\025\001\026\001\
\005\001\028\001\019\001\255\255\009\001\010\001\023\001\024\001\
\013\001\083\000\255\255\016\001\255\255\018\001\019\001\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\005\001\028\001\
\255\255\255\255\009\001\010\001\255\255\255\255\013\001\255\255\
\255\255\016\001\255\255\018\001\019\001\020\001\255\255\022\001\
\023\001\024\001\025\001\026\001\005\001\028\001\255\255\255\255\
\009\001\010\001\255\255\255\255\013\001\255\255\255\255\016\001\
\255\255\018\001\019\001\020\001\255\255\022\001\023\001\024\001\
\005\001\026\001\255\255\028\001\009\001\010\001\255\255\255\255\
\013\001\255\255\255\255\016\001\255\255\018\001\019\001\020\001\
\255\255\022\001\023\001\024\001\005\001\026\001\255\255\028\001\
\009\001\010\001\255\255\255\255\013\001\255\255\255\255\016\001\
\255\255\018\001\019\001\020\001\255\255\022\001\023\001\024\001\
\255\255\026\001\255\255\028\001\001\001\002\001\003\001\004\001\
\255\255\255\255\255\255\008\001\255\255\255\255\011\001\255\255\
\255\255\255\255\015\001\255\255\017\001\255\255\255\255\255\255\
\021\001\255\255\255\255\024\001\001\001\002\001\003\001\004\001\
\255\255\255\255\255\255\008\001\255\255\255\255\255\255\255\255\
\255\255\255\255\015\001\255\255\017\001\018\001\255\255\255\255\
\021\001\005\001\255\255\024\001\255\255\009\001\010\001\255\255\
\255\255\013\001\009\001\010\001\016\001\255\255\018\001\255\255\
\020\001\016\001\022\001\018\001\255\255\020\001\026\001\022\001\
\028\001\001\001\002\001\003\001\004\001\028\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\005\001\015\001\
\255\255\017\001\009\001\010\001\255\255\021\001\255\255\255\255\
\024\001\016\001\255\255\018\001\255\255\020\001\255\255\022\001\
\255\255\255\255\255\255\255\255\255\255\028\001"

let yynames_const = "\
  FUNCTION\000\
  PIPE\000\
  WILDCARD\000\
  FN_ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  MATCH\000\
  WITH\000\
  LBRACK\000\
  RBRACK\000\
  CONS\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  GT\000\
  COLON\000\
  EOD\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 36 "parser.mly"
           ( print ";;"; _1 )
# 261 "parser.ml"
               : modecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 39 "parser.mly"
        ( print "d -> e"; Expr(_1) )
# 268 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 40 "parser.mly"
                   ( print "d -> let x = e"; Let(_2,_4) )
# 276 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 42 "parser.mly"
      ( print "d -> let rec f p = e"; LetRec(_3,_4,_6) )
# 285 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compareexp) in
    Obj.repr(
# 46 "parser.mly"
               ( _1 )
# 292 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 47 "parser.mly"
                                  ( print "e -> function p -> e"; Function(_2,_4) )
# 300 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patterns) in
    Obj.repr(
# 48 "parser.mly"
                               ( print "e -> match e with ps"; Match(_2,_4) )
# 308 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 49 "parser.mly"
                             ( print "e -> if e then e else e"; If(_2,_4,_6) )
# 317 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 53 "parser.mly"
           ( _1 )
# 324 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 54 "parser.mly"
                          ( BinOp(_1,Eq,_3) )
# 332 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 55 "parser.mly"
                          ( BinOp(_1,Gt,_3) )
# 340 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 58 "parser.mly"
            ( _1 )
# 347 "parser.ml"
               : 'consexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 59 "parser.mly"
                         ( BinOp(_1,Cons,_3) )
# 355 "parser.ml"
               : 'consexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 63 "parser.mly"
             ( _1 )
# 362 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 64 "parser.mly"
                          ( BinOp(_1,Plus,_3) )
# 370 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 65 "parser.mly"
                           ( BinOp(_1,Minus,_3) )
# 378 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'negexp) in
    Obj.repr(
# 69 "parser.mly"
           ( _1 )
# 385 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'timesexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 70 "parser.mly"
                          ( BinOp(_1,Times,_3) )
# 393 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 74 "parser.mly"
           ( _1 )
# 400 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 75 "parser.mly"
                 ( Negate(_2) )
# 407 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 79 "parser.mly"
            ( print "ae -> be"; _1 )
# 414 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 80 "parser.mly"
                   ( print "ae -> ae ae"; FunctionCall(_1,_2) )
# 422 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 83 "parser.mly"
        ( print "be -> c"; _1 )
# 429 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
          ( print ("be -> var "^_1); Var(_1) )
# 436 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'explist) in
    Obj.repr(
# 85 "parser.mly"
                          ( _2 )
# 443 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 86 "parser.mly"
                      ( print "be -> (e)"; _2 )
# 450 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
         ( print ("c -> int "^(string_of_int _1)); IntConst(_1) )
# 457 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 91 "parser.mly"
         ( print ("c -> bool "^(string_of_bool _1)); BoolConst(_1) )
# 464 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                  ( print "c -> []"; Nil )
# 470 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 96 "parser.mly"
      ( BinOp(_1, Cons, Nil) )
# 477 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'explist) in
    Obj.repr(
# 97 "parser.mly"
                        ( BinOp(_1, Cons, _3) )
# 485 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 101 "parser.mly"
                         ( print "fp -> p -> e"; [(_1,_3)] )
# 493 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 102 "parser.mly"
                                        ( print "fp -> e fpA"; (_1,_3) :: _4 )
# 502 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 106 "parser.mly"
                              ( print "fpA -> | p -> e"; [(_2,_4)] )
# 510 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 107 "parser.mly"
                                             ( print "fpA -> | p -> e fpA"; (_2,_4) :: _5 )
# 519 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basepat) in
    Obj.repr(
# 111 "parser.mly"
            ( print "p -> bp"; _1 )
# 526 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basepat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 112 "parser.mly"
                         ( print "p -> p1::p2"; ConsPat(_1,_3) )
# 534 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 116 "parser.mly"
          ( print "p -> c";
				match _1 with
					 IntConst(i)  -> IntPat(i)
				  | BoolConst(b) -> BoolPat(b)
				  | Nil -> NilPat
				  | _ -> raise ConstTypeParseError )
# 546 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
             ( print "p -> _"; WildcardPat )
# 552 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
        ( print ("p -> var "^_1^":t"); VarPat(_1) )
# 559 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 124 "parser.mly"
                          ( print "p -> (p)"; _2 )
# 566 "parser.ml"
               : 'basepat))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : modecl)
