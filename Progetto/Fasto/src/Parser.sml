local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = char*(int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = string*(int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = int*(int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = (int*int)
type t__32__ = string*(int*int)
type t__33__ = (int*int)
type t__34__ = (int*int)
type t__35__ = (int*int)
type t__36__ = (int*int)
in
datatype token =
    AND of t__1__
  | BOOL of t__2__
  | CHAR of t__3__
  | CHARLIT of t__4__
  | COMMA of t__5__
  | DEQ of t__6__
  | DIVIDE of t__7__
  | ELSE of t__8__
  | EOF of t__9__
  | EQ of t__10__
  | FALSE of t__11__
  | FUN of t__12__
  | ID of t__13__
  | IF of t__14__
  | IN of t__15__
  | INT of t__16__
  | LBRACKET of t__17__
  | LCURLY of t__18__
  | LET of t__19__
  | LPAR of t__20__
  | LTH of t__21__
  | MINUS of t__22__
  | NEGATE of t__23__
  | NOT of t__24__
  | NUM of t__25__
  | OR of t__26__
  | PLUS of t__27__
  | RBRACKET of t__28__
  | RCURLY of t__29__
  | READ of t__30__
  | RPAR of t__31__
  | STRINGLIT of t__32__
  | THEN of t__33__
  | TIMES of t__34__
  | TRUE of t__35__
  | WRITE of t__36__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";


(* A parser definition for Fasto, for use with mosmlyac. *)

open Fasto
open Fasto.UnknownTypes

(* Line 12, file Parser.sml *)
val yytransl = #[
  257 (* AND *),
  258 (* BOOL *),
  259 (* CHAR *),
  260 (* CHARLIT *),
  261 (* COMMA *),
  262 (* DEQ *),
  263 (* DIVIDE *),
  264 (* ELSE *),
  265 (* EOF *),
  266 (* EQ *),
  267 (* FALSE *),
  268 (* FUN *),
  269 (* ID *),
  270 (* IF *),
  271 (* IN *),
  272 (* INT *),
  273 (* LBRACKET *),
  274 (* LCURLY *),
  275 (* LET *),
  276 (* LPAR *),
  277 (* LTH *),
  278 (* MINUS *),
  279 (* NEGATE *),
  280 (* NOT *),
  281 (* NUM *),
  282 (* OR *),
  283 (* PLUS *),
  284 (* RBRACKET *),
  285 (* RCURLY *),
  286 (* READ *),
  287 (* RPAR *),
  288 (* STRINGLIT *),
  289 (* THEN *),
  290 (* TIMES *),
  291 (* TRUE *),
  292 (* WRITE *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\\004\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\007\000\007\000\000\000";

val yylen = "\002\000\
\\002\000\003\000\002\000\007\000\006\000\001\000\001\000\001\000\
\\003\000\004\000\002\000\001\000\001\000\001\000\001\000\001\000\
\\001\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\\003\000\002\000\003\000\003\000\006\000\004\000\003\000\004\000\
\\004\000\003\000\006\000\004\000\003\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\039\000\000\000\007\000\008\000\006\000\
\\000\000\000\000\000\000\001\000\000\000\002\000\000\000\009\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\000\000\000\015\000\016\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\010\000\000\000\000\000\031\000\000\000\
\\000\000\000\000\018\000\000\000\034\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\\030\000\000\000\037\000\000\000\032\000\033\000\000\000\000\000\
\\000\000\000\000";

val yydgoto = "\002\000\
\\004\000\005\000\010\000\019\000\020\000\044\000\045\000";

val yysindex = "\022\000\
\\247\254\000\000\056\255\000\000\041\255\000\000\000\000\000\000\
\\056\255\247\254\038\255\000\000\033\255\000\000\042\255\000\000\
\\010\255\054\255\052\255\037\255\084\000\066\255\068\255\000\000\
\\000\000\244\254\084\000\084\000\070\255\084\000\084\000\084\000\
\\000\000\069\255\000\000\000\000\072\255\225\255\056\255\084\000\
\\084\000\002\000\127\255\145\255\061\255\083\255\156\255\225\255\
\\225\255\056\255\084\000\084\000\084\000\084\000\084\000\084\000\
\\084\000\084\000\084\000\000\000\225\255\167\255\000\000\064\255\
\\084\000\084\000\000\000\084\000\000\000\065\255\185\255\018\255\
\\251\254\000\000\251\254\008\255\236\255\008\255\000\000\000\000\
\\000\000\196\255\000\000\214\255\000\000\000\000\084\000\084\000\
\\225\255\225\255";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\088\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\067\255\000\000\000\000\
\\000\000\048\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\009\255\000\000\000\000\
\\000\000\000\000\000\000\241\254\000\000\000\000\000\000\187\000\
\\199\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\034\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\160\000\
\\116\000\079\255\145\000\034\000\172\000\063\000\110\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\214\000\226\000";

val yygindex = "\000\000\
\\000\000\089\000\000\000\254\255\064\000\235\255\218\255";

val YYTABLESIZE = 515;
val yytable = "\038\000\
\\011\000\054\000\003\000\064\000\041\000\043\000\013\000\042\000\
\\047\000\048\000\049\000\006\000\007\000\038\000\054\000\038\000\
\\056\000\005\000\061\000\062\000\005\000\058\000\001\000\053\000\
\\054\000\008\000\009\000\083\000\059\000\071\000\072\000\073\000\
\\074\000\075\000\076\000\077\000\078\000\079\000\055\000\056\000\
\\018\000\059\000\004\000\082\000\058\000\004\000\084\000\070\000\
\\014\000\012\000\015\000\059\000\014\000\014\000\014\000\014\000\
\\014\000\006\000\007\000\014\000\016\000\017\000\014\000\021\000\
\\022\000\089\000\090\000\023\000\014\000\014\000\039\000\008\000\
\\009\000\014\000\014\000\014\000\014\000\040\000\014\000\021\000\
\\014\000\014\000\046\000\021\000\021\000\021\000\021\000\021\000\
\\050\000\067\000\021\000\051\000\068\000\021\000\081\000\085\000\
\\003\000\011\000\014\000\021\000\021\000\000\000\060\000\000\000\
\\021\000\021\000\021\000\021\000\000\000\021\000\020\000\021\000\
\\021\000\000\000\020\000\020\000\020\000\020\000\020\000\000\000\
\\000\000\020\000\000\000\000\000\020\000\000\000\000\000\052\000\
\\000\000\000\000\020\000\020\000\053\000\054\000\000\000\020\000\
\\020\000\020\000\020\000\000\000\020\000\000\000\020\000\020\000\
\\000\000\052\000\000\000\055\000\056\000\066\000\053\000\054\000\
\\057\000\058\000\000\000\000\000\052\000\000\000\000\000\065\000\
\\059\000\053\000\054\000\000\000\000\000\055\000\056\000\052\000\
\\000\000\000\000\057\000\058\000\053\000\054\000\000\000\000\000\
\\055\000\056\000\059\000\000\000\000\000\057\000\058\000\000\000\
\\000\000\052\000\069\000\055\000\056\000\059\000\053\000\054\000\
\\057\000\058\000\080\000\000\000\052\000\000\000\000\000\000\000\
\\059\000\053\000\054\000\087\000\000\000\055\000\056\000\000\000\
\\000\000\000\000\057\000\058\000\000\000\000\000\052\000\086\000\
\\055\000\056\000\059\000\053\000\054\000\057\000\058\000\000\000\
\\000\000\052\000\000\000\000\000\088\000\059\000\053\000\054\000\
\\000\000\000\000\055\000\056\000\052\000\000\000\000\000\057\000\
\\058\000\053\000\054\000\000\000\000\000\055\000\056\000\059\000\
\\000\000\000\000\057\000\058\000\000\000\000\000\000\000\000\000\
\\055\000\056\000\059\000\000\000\000\000\024\000\058\000\000\000\
\\000\000\000\000\000\000\000\000\025\000\059\000\026\000\027\000\
\\000\000\000\000\000\000\028\000\029\000\030\000\000\000\000\000\
\\031\000\032\000\033\000\000\000\000\000\000\000\000\000\034\000\
\\063\000\035\000\023\000\000\000\036\000\037\000\023\000\023\000\
\\000\000\023\000\023\000\000\000\000\000\023\000\000\000\000\000\
\\023\000\000\000\000\000\000\000\000\000\000\000\023\000\023\000\
\\000\000\000\000\000\000\023\000\023\000\023\000\023\000\022\000\
\\023\000\000\000\023\000\022\000\022\000\000\000\022\000\022\000\
\\000\000\000\000\022\000\000\000\000\000\022\000\000\000\000\000\
\\000\000\000\000\000\000\022\000\022\000\000\000\000\000\024\000\
\\022\000\022\000\022\000\022\000\000\000\022\000\025\000\022\000\
\\026\000\027\000\000\000\000\000\000\000\028\000\029\000\030\000\
\\000\000\000\000\031\000\032\000\033\000\000\000\000\000\000\000\
\\000\000\034\000\000\000\035\000\024\000\000\000\036\000\037\000\
\\024\000\024\000\000\000\024\000\024\000\000\000\000\000\024\000\
\\000\000\000\000\024\000\000\000\000\000\000\000\000\000\000\000\
\\024\000\000\000\000\000\000\000\000\000\024\000\000\000\024\000\
\\024\000\025\000\024\000\000\000\024\000\025\000\025\000\000\000\
\\025\000\025\000\000\000\000\000\025\000\000\000\000\000\025\000\
\\027\000\000\000\000\000\000\000\027\000\025\000\000\000\027\000\
\\027\000\000\000\025\000\027\000\025\000\025\000\027\000\025\000\
\\028\000\025\000\000\000\028\000\028\000\000\000\000\000\028\000\
\\000\000\027\000\028\000\027\000\027\000\000\000\027\000\019\000\
\\027\000\000\000\019\000\019\000\000\000\028\000\019\000\028\000\
\\028\000\019\000\028\000\026\000\028\000\000\000\026\000\026\000\
\\000\000\000\000\026\000\000\000\000\000\026\000\019\000\019\000\
\\000\000\019\000\029\000\019\000\000\000\029\000\029\000\000\000\
\\000\000\029\000\026\000\026\000\029\000\026\000\035\000\026\000\
\\000\000\035\000\035\000\000\000\000\000\035\000\000\000\000\000\
\\035\000\029\000\029\000\000\000\029\000\000\000\029\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\035\000\035\000\000\000\
\\035\000\000\000\035\000";

val yycheck = "\021\000\
\\003\000\007\001\012\001\042\000\017\001\027\000\009\000\020\001\
\\030\000\031\000\032\000\002\001\003\001\029\001\007\001\031\001\
\\022\001\009\001\040\000\041\000\012\001\027\001\001\000\006\001\
\\007\001\016\001\017\001\066\000\034\001\051\000\052\000\053\000\
\\054\000\055\000\056\000\057\000\058\000\059\000\021\001\022\001\
\\031\001\034\001\009\001\065\000\027\001\012\001\068\000\050\000\
\\001\001\009\001\013\001\034\001\005\001\006\001\007\001\008\001\
\\009\001\002\001\003\001\012\001\028\001\020\001\015\001\010\001\
\\013\001\087\000\088\000\031\001\021\001\022\001\005\001\016\001\
\\017\001\026\001\027\001\028\001\029\001\010\001\031\001\001\001\
\\033\001\034\001\013\001\005\001\006\001\007\001\008\001\009\001\
\\020\001\029\001\012\001\020\001\010\001\015\001\031\001\031\001\
\\009\001\031\001\010\000\021\001\022\001\255\255\039\000\255\255\
\\026\001\027\001\028\001\029\001\255\255\031\001\001\001\033\001\
\\034\001\255\255\005\001\006\001\007\001\008\001\009\001\255\255\
\\255\255\012\001\255\255\255\255\015\001\255\255\255\255\001\001\
\\255\255\255\255\021\001\022\001\006\001\007\001\255\255\026\001\
\\027\001\028\001\029\001\255\255\031\001\255\255\033\001\034\001\
\\255\255\001\001\255\255\021\001\022\001\005\001\006\001\007\001\
\\026\001\027\001\255\255\255\255\001\001\255\255\255\255\033\001\
\\034\001\006\001\007\001\255\255\255\255\021\001\022\001\001\001\
\\255\255\255\255\026\001\027\001\006\001\007\001\255\255\255\255\
\\021\001\022\001\034\001\255\255\255\255\026\001\027\001\255\255\
\\255\255\001\001\031\001\021\001\022\001\034\001\006\001\007\001\
\\026\001\027\001\028\001\255\255\001\001\255\255\255\255\255\255\
\\034\001\006\001\007\001\008\001\255\255\021\001\022\001\255\255\
\\255\255\255\255\026\001\027\001\255\255\255\255\001\001\031\001\
\\021\001\022\001\034\001\006\001\007\001\026\001\027\001\255\255\
\\255\255\001\001\255\255\255\255\015\001\034\001\006\001\007\001\
\\255\255\255\255\021\001\022\001\001\001\255\255\255\255\026\001\
\\027\001\006\001\007\001\255\255\255\255\021\001\022\001\034\001\
\\255\255\255\255\026\001\027\001\255\255\255\255\255\255\255\255\
\\021\001\022\001\034\001\255\255\255\255\004\001\027\001\255\255\
\\255\255\255\255\255\255\255\255\011\001\034\001\013\001\014\001\
\\255\255\255\255\255\255\018\001\019\001\020\001\255\255\255\255\
\\023\001\024\001\025\001\255\255\255\255\255\255\255\255\030\001\
\\031\001\032\001\001\001\255\255\035\001\036\001\005\001\006\001\
\\255\255\008\001\009\001\255\255\255\255\012\001\255\255\255\255\
\\015\001\255\255\255\255\255\255\255\255\255\255\021\001\022\001\
\\255\255\255\255\255\255\026\001\027\001\028\001\029\001\001\001\
\\031\001\255\255\033\001\005\001\006\001\255\255\008\001\009\001\
\\255\255\255\255\012\001\255\255\255\255\015\001\255\255\255\255\
\\255\255\255\255\255\255\021\001\022\001\255\255\255\255\004\001\
\\026\001\027\001\028\001\029\001\255\255\031\001\011\001\033\001\
\\013\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\\255\255\255\255\023\001\024\001\025\001\255\255\255\255\255\255\
\\255\255\030\001\255\255\032\001\001\001\255\255\035\001\036\001\
\\005\001\006\001\255\255\008\001\009\001\255\255\255\255\012\001\
\\255\255\255\255\015\001\255\255\255\255\255\255\255\255\255\255\
\\021\001\255\255\255\255\255\255\255\255\026\001\255\255\028\001\
\\029\001\001\001\031\001\255\255\033\001\005\001\006\001\255\255\
\\008\001\009\001\255\255\255\255\012\001\255\255\255\255\015\001\
\\001\001\255\255\255\255\255\255\005\001\021\001\255\255\008\001\
\\009\001\255\255\026\001\012\001\028\001\029\001\015\001\031\001\
\\005\001\033\001\255\255\008\001\009\001\255\255\255\255\012\001\
\\255\255\026\001\015\001\028\001\029\001\255\255\031\001\005\001\
\\033\001\255\255\008\001\009\001\255\255\026\001\012\001\028\001\
\\029\001\015\001\031\001\005\001\033\001\255\255\008\001\009\001\
\\255\255\255\255\012\001\255\255\255\255\015\001\028\001\029\001\
\\255\255\031\001\005\001\033\001\255\255\008\001\009\001\255\255\
\\255\255\012\001\028\001\029\001\015\001\031\001\005\001\033\001\
\\255\255\008\001\009\001\255\255\255\255\012\001\255\255\255\255\
\\015\001\028\001\029\001\255\255\031\001\255\255\033\001\255\255\
\\255\255\255\255\255\255\255\255\255\255\028\001\029\001\255\255\
\\031\001\255\255\033\001";

val yyact = vector_ 40 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 42 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.UnknownTypes.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : Fasto.UnknownTypes.Prog))
;
(* Rule 2, file Parser.grm, line 45 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.FunDec
val d__3__ = peekVal 0 : Fasto.UnknownTypes.FunDec list
in
( (d__2__) :: (d__3__) ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 3, file Parser.grm, line 46 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.FunDec
in
( (d__2__) :: [] ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 4, file Parser.grm, line 50 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 6 : Fasto.Type
val d__2__ = peekVal 5 : string*(int*int)
val d__3__ = peekVal 4 : (int*int)
val d__4__ = peekVal 3 : Fasto.Param list
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( FunDec (#1 (d__2__), (d__1__), (d__4__), (d__7__), #2 (d__2__)) ) end : Fasto.UnknownTypes.FunDec))
;
(* Rule 5, file Parser.grm, line 52 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 5 : Fasto.Type
val d__2__ = peekVal 4 : string*(int*int)
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : (int*int)
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( FunDec (#1 (d__2__), (d__1__), [], (d__6__), #2 (d__2__)) ) end : Fasto.UnknownTypes.FunDec))
;
(* Rule 6, file Parser.grm, line 55 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Int ) end : Fasto.Type))
;
(* Rule 7, file Parser.grm, line 56 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Bool ) end : Fasto.Type))
;
(* Rule 8, file Parser.grm, line 57 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Char ) end : Fasto.Type))
;
(* Rule 9, file Parser.grm, line 58 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.Type
val d__3__ = peekVal 0 : (int*int)
in
( Array (d__2__) ) end : Fasto.Type))
;
(* Rule 10, file Parser.grm, line 62 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 3 : Fasto.Type
val d__2__ = peekVal 2 : string*(int*int)
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Fasto.Param list
in
( Param (#1 (d__2__), (d__1__)) :: (d__4__) ) end : Fasto.Param list))
;
(* Rule 11, file Parser.grm, line 63 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.Type
val d__2__ = peekVal 0 : string*(int*int)
in
( Param (#1 (d__2__), (d__1__)) :: [] ) end : Fasto.Param list))
;
(* Rule 12, file Parser.grm, line 66 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( Constant (IntVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 13, file Parser.grm, line 67 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 0 : char*(int*int)
in
( Constant (CharVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 14, file Parser.grm, line 68 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( Var (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 15, file Parser.grm, line 69 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( StringLit (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 16, file Parser.grm, line 70 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Constant ((BoolVal true), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 17, file Parser.grm, line 71 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Constant ((BoolVal false), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 18, file Parser.grm, line 73 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__3__ = peekVal 0 : (int*int)
in
( ArrayLit ((d__2__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 19, file Parser.grm, line 74 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Negate ((d__2__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 20, file Parser.grm, line 75 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Times ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 21, file Parser.grm, line 76 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Divide ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 22, file Parser.grm, line 77 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Plus ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 23, file Parser.grm, line 78 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Minus((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 24, file Parser.grm, line 79 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Equal((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 25, file Parser.grm, line 80 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Less ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 26, file Parser.grm, line 81 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Not ((d__2__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 27, file Parser.grm, line 82 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( And ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 28, file Parser.grm, line 83 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Or ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 29, file Parser.grm, line 85 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : Fasto.UnknownTypes.Exp
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( If ((d__2__), (d__4__), (d__6__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 30, file Parser.grm, line 87 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), (d__3__), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 31, file Parser.grm, line 89 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), [], #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 32, file Parser.grm, line 92 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.Type
val d__4__ = peekVal 0 : (int*int)
in
( Read ((d__3__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 33, file Parser.grm, line 94 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Write ((d__3__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 34, file Parser.grm, line 96 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 35, file Parser.grm, line 98 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : string*(int*int)
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Let (Dec (#1 (d__2__), (d__4__), (d__3__)), (d__6__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 36, file Parser.grm, line 100 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Index (#1 (d__1__), (d__3__), (), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 37, file Parser.grm, line 103 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp list
in
( (d__1__) :: (d__3__) ) end : Fasto.UnknownTypes.Exp list))
;
(* Rule 38, file Parser.grm, line 104 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( (d__1__) :: [] ) end : Fasto.UnknownTypes.Exp list))
;
(* Entry Prog *)
val _ = update_ yyact 39 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Prog lexer lexbuf = yyparse yytables 1 lexer lexbuf;
