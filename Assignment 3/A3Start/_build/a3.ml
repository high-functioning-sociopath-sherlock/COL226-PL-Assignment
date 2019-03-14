type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | ABS
  | TILDA
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | COMMA
  | PROJ
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open A1
	exception Foo of string  

    let getf a = match a with
                |(a1, a2) -> a1

    
    let gets a = match a with
                |(a1, a2) -> a2
# 41 "a3.ml"
let yytransl_const = [|
  260 (* ABS *);
  261 (* TILDA *);
  262 (* NOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* REM *);
  268 (* CONJ *);
  269 (* DISJ *);
  270 (* EQ *);
  271 (* GT *);
  272 (* LT *);
  273 (* LP *);
  274 (* RP *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* COMMA *);
  280 (* PROJ *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\006\000\007\000\007\000\007\000\007\000\008\000\008\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\013\000\
\013\000\014\000\014\000\015\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\001\000\003\000\001\000\002\000\001\000\
\004\000\004\000\003\000\003\000\003\000\001\000\003\000\003\000\
\001\000\003\000\003\000\003\000\001\000\002\000\001\000\002\000\
\001\000\007\000\001\000\007\000\001\000\003\000\001\000\003\000\
\001\000\003\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\037\000\038\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\039\000\000\000\000\000\006\000\
\000\000\000\000\000\000\021\000\023\000\025\000\027\000\029\000\
\031\000\035\000\022\000\024\000\007\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\030\000\000\000\
\000\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\020\000\018\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\031\000\025\000\026\000"

let yysindex = "\007\000\
\001\000\000\000\000\000\000\000\000\000\020\255\073\255\010\255\
\010\255\010\255\003\255\000\000\000\000\002\000\021\255\000\000\
\065\255\254\254\094\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\076\255\012\255\018\255\
\031\255\010\255\000\000\010\255\020\255\047\255\053\255\020\255\
\020\255\020\255\020\255\020\255\000\000\010\255\000\000\010\255\
\036\255\021\255\000\000\254\254\020\255\254\254\020\255\254\254\
\094\255\094\255\000\000\000\000\000\000\250\254\000\000\253\254\
\040\255\254\254\254\254\010\255\025\255\013\255\010\255\000\000\
\050\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\160\000\068\000\177\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\083\000\000\000\098\000\000\000\113\000\
\194\000\211\000\000\000\000\000\000\000\042\255\000\000\000\000\
\000\000\128\000\143\000\000\000\000\000\000\000\000\000\000\000\
\053\000"

let yygindex = "\000\000\
\000\000\250\255\035\000\248\255\000\000\047\000\055\000\003\000\
\066\000\000\000\000\000\000\000\041\000\000\000\000\000"

let yytablesize = 490
let yytable = "\029\000\
\012\000\035\000\030\000\032\000\040\000\041\000\034\000\001\000\
\027\000\034\000\003\000\004\000\005\000\006\000\007\000\008\000\
\046\000\068\000\004\000\033\000\003\000\004\000\005\000\006\000\
\007\000\034\000\009\000\051\000\010\000\047\000\034\000\049\000\
\036\000\011\000\072\000\003\000\009\000\048\000\010\000\062\000\
\069\000\064\000\071\000\011\000\059\000\060\000\061\000\003\000\
\004\000\005\000\006\000\007\000\028\000\003\000\004\000\005\000\
\006\000\007\000\065\000\033\000\053\000\070\000\034\000\009\000\
\073\000\010\000\055\000\014\000\050\000\009\000\011\000\010\000\
\028\000\003\000\004\000\005\000\011\000\007\000\037\000\038\000\
\039\000\000\000\011\000\052\000\054\000\056\000\063\000\000\000\
\034\000\009\000\000\000\010\000\000\000\045\000\057\000\058\000\
\011\000\013\000\046\000\066\000\000\000\067\000\042\000\043\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\009\000\000\000\010\000\000\000\000\000\000\000\000\000\
\011\000\004\000\004\000\004\000\004\000\004\000\000\000\004\000\
\004\000\004\000\004\000\000\000\004\000\000\000\004\000\004\000\
\004\000\004\000\003\000\003\000\003\000\003\000\003\000\000\000\
\003\000\003\000\003\000\003\000\000\000\003\000\000\000\003\000\
\003\000\003\000\003\000\028\000\028\000\028\000\028\000\028\000\
\028\000\000\000\028\000\028\000\028\000\000\000\028\000\000\000\
\028\000\028\000\028\000\028\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\000\000\014\000\000\000\014\000\
\014\000\014\000\014\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\011\000\000\000\011\000\011\000\
\011\000\011\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000\013\000\000\000\013\000\013\000\013\000\
\013\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000\012\000\000\000\012\000\012\000\012\000\012\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\000\000\010\000\000\000\010\000\010\000\010\000\010\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\000\000\
\009\000\000\000\009\000\009\000\009\000\009\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\000\000\000\000\
\000\000\008\000\000\000\008\000\008\000\008\000\008\000\017\000\
\017\000\000\000\000\000\000\000\017\000\017\000\017\000\017\000\
\017\000\000\000\017\000\000\000\017\000\017\000\017\000\017\000\
\016\000\016\000\000\000\000\000\000\000\016\000\016\000\016\000\
\016\000\016\000\000\000\016\000\000\000\016\000\016\000\016\000\
\016\000\015\000\015\000\000\000\000\000\000\000\015\000\015\000\
\015\000\015\000\015\000\000\000\015\000\000\000\015\000\015\000\
\015\000\015\000"

let yycheck = "\008\000\
\000\000\000\000\009\000\010\000\007\001\008\001\013\001\001\000\
\006\000\013\001\001\001\002\001\003\001\004\001\005\001\006\001\
\023\001\021\001\000\000\017\001\001\001\002\001\003\001\004\001\
\005\001\013\001\017\001\036\000\019\001\018\001\013\001\001\001\
\012\001\024\001\022\001\000\000\017\001\020\001\019\001\046\000\
\001\001\048\000\018\001\024\001\042\000\043\000\044\000\001\001\
\002\001\003\001\004\001\005\001\000\000\001\001\002\001\003\001\
\004\001\005\001\023\001\018\001\014\001\068\000\013\001\017\001\
\071\000\019\001\014\001\000\000\034\000\017\001\024\001\019\001\
\007\000\001\001\002\001\003\001\024\001\005\001\014\001\015\001\
\016\001\255\255\000\000\037\000\038\000\039\000\046\000\255\255\
\013\001\017\001\255\255\019\001\255\255\018\001\040\000\041\000\
\024\001\000\000\023\001\053\000\255\255\055\000\009\001\010\001\
\011\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\255\255\
\255\255\017\001\255\255\019\001\255\255\255\255\255\255\255\255\
\024\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\014\001\015\001\016\001\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\255\255\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001"

let yynames_const = "\
  ABS\000\
  TILDA\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GT\000\
  LT\000\
  LP\000\
  RP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  COMMA\000\
  PROJ\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bool_disjunction) in
    Obj.repr(
# 34 "a3.mly"
                                       ( _1 )
# 298 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "a3.mly"
                                       ( raise(Foo "Your tree is empty") )
# 304 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_disjunction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_conjunction) in
    Obj.repr(
# 39 "a3.mly"
                                             ( Disjunction(_1, _3))
# 312 "a3.ml"
               : 'bool_disjunction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_conjunction) in
    Obj.repr(
# 40 "a3.mly"
                                             ( _1 )
# 319 "a3.ml"
               : 'bool_disjunction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_conjunction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_not) in
    Obj.repr(
# 44 "a3.mly"
                                         ( Conjunction(_1, _3) )
# 327 "a3.ml"
               : 'bool_conjunction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_not) in
    Obj.repr(
# 45 "a3.mly"
                                         ( _1 )
# 334 "a3.ml"
               : 'bool_conjunction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_not) in
    Obj.repr(
# 49 "a3.mly"
                                          ( Not(_2))
# 341 "a3.ml"
               : 'bool_not))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmp_expression) in
    Obj.repr(
# 50 "a3.mly"
                                          ( _1 )
# 348 "a3.ml"
               : 'bool_not))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'cmp_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'as_expression) in
    Obj.repr(
# 54 "a3.mly"
                                         ( LessTE(_1, _4))
# 356 "a3.ml"
               : 'cmp_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'cmp_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'as_expression) in
    Obj.repr(
# 55 "a3.mly"
                                         ( GreaterTE(_1, _4))
# 364 "a3.ml"
               : 'cmp_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmp_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'as_expression) in
    Obj.repr(
# 56 "a3.mly"
                                         ( Equals(_1, _3))
# 372 "a3.ml"
               : 'cmp_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmp_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'as_expression) in
    Obj.repr(
# 57 "a3.mly"
                                         ( LessT(_1, _3))
# 380 "a3.ml"
               : 'cmp_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmp_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'as_expression) in
    Obj.repr(
# 58 "a3.mly"
                                         ( GreaterT(_1, _3))
# 388 "a3.ml"
               : 'cmp_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'as_expression) in
    Obj.repr(
# 59 "a3.mly"
                                         ( _1 )
# 395 "a3.ml"
               : 'cmp_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'as_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'r_expression) in
    Obj.repr(
# 63 "a3.mly"
                                       ( Sub(_1,_3) )
# 403 "a3.ml"
               : 'as_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'as_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'r_expression) in
    Obj.repr(
# 64 "a3.mly"
                                       (Add(_1,_3) )
# 411 "a3.ml"
               : 'as_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'r_expression) in
    Obj.repr(
# 65 "a3.mly"
                                       ( _1 )
# 418 "a3.ml"
               : 'as_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'r_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expression) in
    Obj.repr(
# 75 "a3.mly"
                                         ( Rem(_1,_3) )
# 426 "a3.ml"
               : 'r_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'r_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expression) in
    Obj.repr(
# 76 "a3.mly"
                                         ( Mult(_1,_3) )
# 434 "a3.ml"
               : 'r_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'r_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expression) in
    Obj.repr(
# 77 "a3.mly"
                                         ( Div(_1,_3) )
# 442 "a3.ml"
               : 'r_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expression) in
    Obj.repr(
# 78 "a3.mly"
                                         ( _1 )
# 449 "a3.ml"
               : 'r_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expression) in
    Obj.repr(
# 95 "a3.mly"
                                          ( Abs(_2) )
# 456 "a3.ml"
               : 'abs_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'neg_expression) in
    Obj.repr(
# 96 "a3.mly"
                                          ( _1 )
# 463 "a3.ml"
               : 'abs_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'neg_expression) in
    Obj.repr(
# 100 "a3.mly"
                                         ( Negative(_2) )
# 470 "a3.ml"
               : 'neg_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ifthen_expression) in
    Obj.repr(
# 101 "a3.mly"
                                         ( _1 )
# 477 "a3.ml"
               : 'neg_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'bool_disjunction) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'bool_disjunction) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'bool_disjunction) in
    Obj.repr(
# 105 "a3.mly"
                                                                         ( IfThenElse(_2,_4,_6))
# 486 "a3.ml"
               : 'ifthen_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proj_expression) in
    Obj.repr(
# 106 "a3.mly"
                                                                         ( _1 )
# 493 "a3.ml"
               : 'ifthen_expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'bool_disjunction) in
    Obj.repr(
# 110 "a3.mly"
                                                ( Project((_3, _5),_7))
# 502 "a3.ml"
               : 'proj_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_expression) in
    Obj.repr(
# 111 "a3.mly"
                                                                          ( _1 )
# 509 "a3.ml"
               : 'proj_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 114 "a3.mly"
                                             ( Tuple(getf(_2),gets(_2)) )
# 516 "a3.ml"
               : 'tuple_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paren_expression) in
    Obj.repr(
# 115 "a3.mly"
                                             ( _1 )
# 523 "a3.ml"
               : 'tuple_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_disjunction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 119 "a3.mly"
                                             ( let (x, y) = _3 in (x+1, _1::y) )
# 531 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_disjunction) in
    Obj.repr(
# 120 "a3.mly"
                                             ( (0, []) )
# 538 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_disjunction) in
    Obj.repr(
# 124 "a3.mly"
                                          ( InParen(_2) )
# 545 "a3.ml"
               : 'paren_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 125 "a3.mly"
                                         ( _1 )
# 552 "a3.ml"
               : 'paren_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "a3.mly"
                                         ( Var(_1) )
# 559 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 130 "a3.mly"
                                         ( N(_1) )
# 566 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 131 "a3.mly"
                                         ( B(_1) )
# 573 "a3.ml"
               : 'constant))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.exptree)
