local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = char*(int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = string*(int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = int*(int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = string*(int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
in
datatype token =
    BOOL of t__1__
  | CHAR of t__2__
  | CHARLIT of t__3__
  | COMMA of t__4__
  | DEQ of t__5__
  | ELSE of t__6__
  | EOF of t__7__
  | EQ of t__8__
  | FUN of t__9__
  | ID of t__10__
  | IF of t__11__
  | IN of t__12__
  | INT of t__13__
  | LBRACKET of t__14__
  | LCURLY of t__15__
  | LET of t__16__
  | LPAR of t__17__
  | LTH of t__18__
  | MINUS of t__19__
  | NUM of t__20__
  | PLUS of t__21__
  | RBRACKET of t__22__
  | RCURLY of t__23__
  | READ of t__24__
  | RPAR of t__25__
  | STRINGLIT of t__26__
  | THEN of t__27__
  | WRITE of t__28__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";


(* A parser definition for Fasto, for use with mosmlyac. *)

open Fasto
open Fasto.UnknownTypes

(* Line 12, file Parser.sml *)
val yytransl = #[
  257 (* BOOL *),
  258 (* CHAR *),
  259 (* CHARLIT *),
  260 (* COMMA *),
  261 (* DEQ *),
  262 (* ELSE *),
  263 (* EOF *),
  264 (* EQ *),
  265 (* FUN *),
  266 (* ID *),
  267 (* IF *),
  268 (* IN *),
  269 (* INT *),
  270 (* LBRACKET *),
  271 (* LCURLY *),
  272 (* LET *),
  273 (* LPAR *),
  274 (* LTH *),
  275 (* MINUS *),
  276 (* NUM *),
  277 (* PLUS *),
  278 (* RBRACKET *),
  279 (* RCURLY *),
  280 (* READ *),
  281 (* RPAR *),
  282 (* STRINGLIT *),
  283 (* THEN *),
  284 (* WRITE *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\\004\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\007\000\007\000\000\000";

val yylen = "\002\000\
\\002\000\003\000\002\000\007\000\006\000\001\000\001\000\001\000\
\\003\000\004\000\002\000\001\000\001\000\001\000\001\000\003\000\
\\003\000\003\000\003\000\003\000\006\000\004\000\003\000\004\000\
\\004\000\003\000\006\000\004\000\003\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\031\000\000\000\007\000\008\000\006\000\
\\000\000\000\000\000\000\001\000\000\000\002\000\000\000\009\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\000\000\000\000\000\000\000\000\000\000\012\000\000\000\015\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\010\000\000\000\000\000\023\000\000\000\000\000\000\000\
\\016\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\028\000\022\000\000\000\029\000\000\000\024\000\025\000\
\\000\000\000\000\000\000\000\000";

val yydgoto = "\002\000\
\\004\000\005\000\010\000\019\000\020\000\040\000\041\000";

val yysindex = "\013\000\
\\014\255\000\000\016\255\000\000\032\255\000\000\000\000\000\000\
\\016\255\014\255\038\255\000\000\037\255\000\000\043\255\000\000\
\\018\255\053\255\052\255\041\255\054\255\059\255\060\255\000\000\
\\248\254\054\255\054\255\057\255\054\255\000\000\055\255\000\000\
\\056\255\146\255\016\255\054\255\054\255\030\255\017\255\214\255\
\\058\255\067\255\071\255\016\255\054\255\054\255\054\255\054\255\
\\054\255\000\000\146\255\122\255\000\000\062\255\054\255\054\255\
\\000\000\054\255\000\000\068\255\098\255\239\254\239\254\000\000\
\\000\000\000\000\000\000\218\255\000\000\229\255\000\000\000\000\
\\054\255\054\255\146\255\146\255";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\070\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\069\255\000\000\000\000\
\\079\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\003\255\000\000\000\000\000\000\000\000\000\000\244\254\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\042\255\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\151\255\175\255\103\255\
\\127\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\182\255\204\255";

val yygindex = "\000\000\
\\000\000\069\000\000\000\254\255\060\000\235\255\221\255";

val YYTABLESIZE = 250;
val yytable = "\034\000\
\\011\000\048\000\054\000\049\000\039\000\037\000\013\000\043\000\
\\038\000\005\000\030\000\005\000\030\000\001\000\051\000\052\000\
\\006\000\007\000\006\000\007\000\069\000\046\000\003\000\061\000\
\\062\000\063\000\064\000\065\000\008\000\009\000\008\000\009\000\
\\024\000\068\000\047\000\048\000\070\000\049\000\012\000\025\000\
\\026\000\060\000\018\000\055\000\027\000\028\000\029\000\015\000\
\\004\000\030\000\004\000\075\000\076\000\031\000\053\000\032\000\
\\024\000\033\000\016\000\017\000\021\000\022\000\035\000\025\000\
\\026\000\023\000\042\000\036\000\027\000\028\000\029\000\044\000\
\\045\000\030\000\058\000\046\000\003\000\031\000\014\000\032\000\
\\057\000\033\000\014\000\014\000\014\000\014\000\067\000\014\000\
\\047\000\048\000\014\000\049\000\071\000\011\000\050\000\059\000\
\\014\000\014\000\000\000\014\000\014\000\014\000\046\000\014\000\
\\000\000\014\000\018\000\018\000\018\000\018\000\000\000\018\000\
\\000\000\000\000\018\000\047\000\048\000\000\000\049\000\000\000\
\\018\000\018\000\072\000\018\000\018\000\018\000\046\000\018\000\
\\000\000\018\000\017\000\017\000\017\000\017\000\000\000\017\000\
\\000\000\000\000\017\000\047\000\048\000\000\000\049\000\066\000\
\\017\000\017\000\000\000\017\000\017\000\017\000\046\000\017\000\
\\000\000\017\000\019\000\019\000\019\000\019\000\000\000\019\000\
\\000\000\000\000\019\000\047\000\048\000\000\000\049\000\000\000\
\\019\000\000\000\000\000\000\000\019\000\019\000\000\000\019\000\
\\000\000\019\000\020\000\020\000\020\000\020\000\000\000\020\000\
\\000\000\021\000\020\000\021\000\021\000\000\000\021\000\000\000\
\\020\000\021\000\000\000\000\000\020\000\020\000\000\000\020\000\
\\000\000\020\000\000\000\021\000\021\000\000\000\021\000\027\000\
\\021\000\027\000\027\000\000\000\027\000\000\000\000\000\027\000\
\\000\000\056\000\046\000\000\000\000\000\000\000\046\000\073\000\
\\000\000\027\000\027\000\000\000\027\000\000\000\027\000\047\000\
\\048\000\046\000\049\000\047\000\048\000\000\000\049\000\000\000\
\\074\000\000\000\000\000\000\000\000\000\000\000\047\000\048\000\
\\000\000\049\000";

val yycheck = "\021\000\
\\003\000\019\001\038\000\021\001\026\000\014\001\009\000\029\000\
\\017\001\007\001\023\001\009\001\025\001\001\000\036\000\037\000\
\\001\001\002\001\001\001\002\001\056\000\005\001\009\001\045\000\
\\046\000\047\000\048\000\049\000\013\001\014\001\013\001\014\001\
\\003\001\055\000\018\001\019\001\058\000\021\001\007\001\010\001\
\\011\001\044\000\025\001\027\001\015\001\016\001\017\001\010\001\
\\007\001\020\001\009\001\073\000\074\000\024\001\025\001\026\001\
\\003\001\028\001\022\001\017\001\008\001\010\001\004\001\010\001\
\\011\001\025\001\010\001\008\001\015\001\016\001\017\001\017\001\
\\017\001\020\001\008\001\005\001\007\001\024\001\010\000\026\001\
\\023\001\028\001\004\001\005\001\006\001\007\001\025\001\009\001\
\\018\001\019\001\012\001\021\001\025\001\025\001\035\000\025\001\
\\018\001\019\001\255\255\021\001\022\001\023\001\005\001\025\001\
\\255\255\027\001\004\001\005\001\006\001\007\001\255\255\009\001\
\\255\255\255\255\012\001\018\001\019\001\255\255\021\001\255\255\
\\018\001\019\001\025\001\021\001\022\001\023\001\005\001\025\001\
\\255\255\027\001\004\001\005\001\006\001\007\001\255\255\009\001\
\\255\255\255\255\012\001\018\001\019\001\255\255\021\001\022\001\
\\018\001\019\001\255\255\021\001\022\001\023\001\005\001\025\001\
\\255\255\027\001\004\001\005\001\006\001\007\001\255\255\009\001\
\\255\255\255\255\012\001\018\001\019\001\255\255\021\001\255\255\
\\018\001\255\255\255\255\255\255\022\001\023\001\255\255\025\001\
\\255\255\027\001\004\001\005\001\006\001\007\001\255\255\009\001\
\\255\255\004\001\012\001\006\001\007\001\255\255\009\001\255\255\
\\018\001\012\001\255\255\255\255\022\001\023\001\255\255\025\001\
\\255\255\027\001\255\255\022\001\023\001\255\255\025\001\004\001\
\\027\001\006\001\007\001\255\255\009\001\255\255\255\255\012\001\
\\255\255\004\001\005\001\255\255\255\255\255\255\005\001\006\001\
\\255\255\022\001\023\001\255\255\025\001\255\255\027\001\018\001\
\\019\001\005\001\021\001\018\001\019\001\255\255\021\001\255\255\
\\012\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\\255\255\021\001";

val yyact = vector_ 32 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 36 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.UnknownTypes.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : Fasto.UnknownTypes.Prog))
;
(* Rule 2, file Parser.grm, line 39 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.FunDec
val d__3__ = peekVal 0 : Fasto.UnknownTypes.FunDec list
in
( (d__2__) :: (d__3__) ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 3, file Parser.grm, line 40 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.FunDec
in
( (d__2__) :: [] ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 4, file Parser.grm, line 44 *)
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
(* Rule 5, file Parser.grm, line 46 *)
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
(* Rule 6, file Parser.grm, line 49 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Int ) end : Fasto.Type))
;
(* Rule 7, file Parser.grm, line 50 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Bool ) end : Fasto.Type))
;
(* Rule 8, file Parser.grm, line 51 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Char ) end : Fasto.Type))
;
(* Rule 9, file Parser.grm, line 52 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.Type
val d__3__ = peekVal 0 : (int*int)
in
( Array (d__2__) ) end : Fasto.Type))
;
(* Rule 10, file Parser.grm, line 56 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 3 : Fasto.Type
val d__2__ = peekVal 2 : string*(int*int)
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Fasto.Param list
in
( Param (#1 (d__2__), (d__1__)) :: (d__4__) ) end : Fasto.Param list))
;
(* Rule 11, file Parser.grm, line 57 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.Type
val d__2__ = peekVal 0 : string*(int*int)
in
( Param (#1 (d__2__), (d__1__)) :: [] ) end : Fasto.Param list))
;
(* Rule 12, file Parser.grm, line 60 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( Constant (IntVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 13, file Parser.grm, line 61 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 0 : char*(int*int)
in
( Constant (CharVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 14, file Parser.grm, line 62 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( Var (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 15, file Parser.grm, line 63 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( StringLit (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 16, file Parser.grm, line 65 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__3__ = peekVal 0 : (int*int)
in
( ArrayLit ((d__2__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 17, file Parser.grm, line 66 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Plus ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 18, file Parser.grm, line 67 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Minus((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 19, file Parser.grm, line 68 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Equal((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 20, file Parser.grm, line 69 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Less ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 21, file Parser.grm, line 71 *)
val _ = update_ yyact 21
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
(* Rule 22, file Parser.grm, line 73 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), (d__3__), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 23, file Parser.grm, line 75 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), [], #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 24, file Parser.grm, line 78 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.Type
val d__4__ = peekVal 0 : (int*int)
in
( Read ((d__3__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 25, file Parser.grm, line 80 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Write ((d__3__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 26, file Parser.grm, line 82 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 27, file Parser.grm, line 84 *)
val _ = update_ yyact 27
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
(* Rule 28, file Parser.grm, line 86 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Index (#1 (d__1__), (d__3__), (), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 29, file Parser.grm, line 89 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp list
in
( (d__1__) :: (d__3__) ) end : Fasto.UnknownTypes.Exp list))
;
(* Rule 30, file Parser.grm, line 90 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( (d__1__) :: [] ) end : Fasto.UnknownTypes.Exp list))
;
(* Entry Prog *)
val _ = update_ yyact 31 (fn () => raise yyexit (peekVal 0));
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
