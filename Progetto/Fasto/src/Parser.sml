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
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = string*(int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = int*(int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
type t__28__ = string*(int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = (int*int)
type t__32__ = (int*int)
in
datatype token =
    BOOL of t__1__
  | CHAR of t__2__
  | CHARLIT of t__3__
  | COMMA of t__4__
  | DEQ of t__5__
  | DIVIDE of t__6__
  | ELSE of t__7__
  | EOF of t__8__
  | EQ of t__9__
  | FALSE of t__10__
  | FUN of t__11__
  | ID of t__12__
  | IF of t__13__
  | IN of t__14__
  | INT of t__15__
  | LBRACKET of t__16__
  | LCURLY of t__17__
  | LET of t__18__
  | LPAR of t__19__
  | LTH of t__20__
  | MINUS of t__21__
  | NUM of t__22__
  | PLUS of t__23__
  | RBRACKET of t__24__
  | RCURLY of t__25__
  | READ of t__26__
  | RPAR of t__27__
  | STRINGLIT of t__28__
  | THEN of t__29__
  | TIMES of t__30__
  | TRUE of t__31__
  | WRITE of t__32__
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
  262 (* DIVIDE *),
  263 (* ELSE *),
  264 (* EOF *),
  265 (* EQ *),
  266 (* FALSE *),
  267 (* FUN *),
  268 (* ID *),
  269 (* IF *),
  270 (* IN *),
  271 (* INT *),
  272 (* LBRACKET *),
  273 (* LCURLY *),
  274 (* LET *),
  275 (* LPAR *),
  276 (* LTH *),
  277 (* MINUS *),
  278 (* NUM *),
  279 (* PLUS *),
  280 (* RBRACKET *),
  281 (* RCURLY *),
  282 (* READ *),
  283 (* RPAR *),
  284 (* STRINGLIT *),
  285 (* THEN *),
  286 (* TIMES *),
  287 (* TRUE *),
  288 (* WRITE *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\\004\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\007\000\007\000\000\000";

val yylen = "\002\000\
\\002\000\003\000\002\000\007\000\006\000\001\000\001\000\001\000\
\\003\000\004\000\002\000\001\000\001\000\001\000\001\000\001\000\
\\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\\006\000\004\000\003\000\004\000\004\000\003\000\006\000\004\000\
\\003\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\035\000\000\000\007\000\008\000\006\000\
\\000\000\000\000\000\000\001\000\000\000\002\000\000\000\009\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\017\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
\\015\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\\027\000\000\000\000\000\000\000\018\000\000\000\030\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\\026\000\000\000\033\000\000\000\028\000\029\000\000\000\000\000\
\\000\000\000\000";

val yydgoto = "\002\000\
\\004\000\005\000\010\000\019\000\020\000\042\000\043\000";

val yysindex = "\002\000\
\\249\254\000\000\035\255\000\000\006\255\000\000\000\000\000\000\
\\035\255\249\254\009\255\000\000\248\254\000\000\024\255\000\000\
\\018\255\038\255\036\255\030\255\069\255\059\255\055\255\000\000\
\\000\000\245\254\069\255\069\255\054\255\069\255\000\000\048\255\
\\000\000\000\000\049\255\064\000\035\255\069\255\069\255\043\255\
\\087\255\019\255\051\255\064\255\025\000\035\255\069\255\069\255\
\\069\255\069\255\069\255\069\255\069\255\000\000\064\000\030\000\
\\000\000\050\255\069\255\069\255\000\000\069\255\000\000\053\255\
\\051\000\090\255\000\000\090\255\005\255\005\255\000\000\000\000\
\\000\000\056\000\000\000\077\000\000\000\000\000\069\255\069\255\
\\064\000\064\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\070\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\056\255\000\000\000\000\
\\000\000\098\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\002\255\000\000\000\000\000\000\000\000\
\\000\000\027\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\004\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\213\255\125\255\239\255\179\255\205\255\152\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\247\255\015\000";

val yygindex = "\000\000\
\\000\000\074\000\000\000\254\255\048\000\235\255\218\255";

val YYTABLESIZE = 363;
val yytable = "\036\000\
\\011\000\058\000\001\000\003\000\039\000\041\000\013\000\040\000\
\\045\000\005\000\049\000\004\000\005\000\012\000\004\000\016\000\
\\055\000\056\000\006\000\007\000\015\000\075\000\060\000\048\000\
\\049\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\\008\000\009\000\053\000\006\000\007\000\074\000\050\000\051\000\
\\076\000\052\000\017\000\064\000\018\000\024\000\021\000\022\000\
\\053\000\008\000\009\000\034\000\025\000\034\000\026\000\027\000\
\\023\000\081\000\082\000\028\000\029\000\030\000\037\000\038\000\
\\031\000\044\000\046\000\047\000\032\000\057\000\033\000\024\000\
\\062\000\034\000\035\000\061\000\073\000\003\000\025\000\077\000\
\\026\000\027\000\011\000\014\000\054\000\028\000\029\000\030\000\
\\000\000\000\000\031\000\048\000\049\000\000\000\032\000\049\000\
\\033\000\000\000\000\000\034\000\035\000\014\000\014\000\014\000\
\\014\000\014\000\050\000\051\000\014\000\052\000\051\000\014\000\
\\052\000\000\000\000\000\059\000\053\000\014\000\014\000\053\000\
\\014\000\014\000\014\000\000\000\014\000\000\000\014\000\014\000\
\\020\000\020\000\020\000\020\000\020\000\000\000\000\000\020\000\
\\000\000\000\000\020\000\000\000\000\000\000\000\000\000\000\000\
\\020\000\020\000\000\000\020\000\020\000\020\000\000\000\020\000\
\\000\000\020\000\020\000\019\000\019\000\019\000\019\000\019\000\
\\000\000\000\000\019\000\000\000\000\000\019\000\000\000\000\000\
\\000\000\000\000\000\000\019\000\019\000\000\000\019\000\019\000\
\\019\000\000\000\019\000\000\000\019\000\019\000\022\000\022\000\
\\000\000\022\000\022\000\000\000\000\000\022\000\000\000\000\000\
\\022\000\000\000\000\000\000\000\000\000\000\000\022\000\022\000\
\\000\000\022\000\022\000\022\000\000\000\022\000\000\000\022\000\
\\021\000\021\000\000\000\021\000\021\000\000\000\000\000\021\000\
\\023\000\023\000\021\000\023\000\023\000\000\000\000\000\023\000\
\\021\000\021\000\023\000\021\000\021\000\021\000\000\000\021\000\
\\023\000\021\000\000\000\000\000\023\000\023\000\000\000\023\000\
\\000\000\023\000\024\000\024\000\000\000\024\000\024\000\000\000\
\\000\000\024\000\025\000\000\000\024\000\025\000\025\000\000\000\
\\000\000\025\000\024\000\000\000\025\000\000\000\024\000\024\000\
\\000\000\024\000\000\000\024\000\000\000\000\000\025\000\025\000\
\\000\000\025\000\031\000\025\000\000\000\031\000\031\000\000\000\
\\000\000\031\000\000\000\000\000\031\000\048\000\049\000\000\000\
\\000\000\000\000\048\000\049\000\000\000\000\000\031\000\031\000\
\\000\000\031\000\000\000\031\000\050\000\051\000\000\000\052\000\
\\000\000\050\000\051\000\063\000\052\000\072\000\053\000\048\000\
\\049\000\000\000\000\000\053\000\048\000\049\000\079\000\000\000\
\\000\000\000\000\000\000\000\000\048\000\049\000\050\000\051\000\
\\000\000\052\000\000\000\050\000\051\000\078\000\052\000\000\000\
\\053\000\048\000\049\000\050\000\051\000\053\000\052\000\000\000\
\\000\000\000\000\080\000\000\000\000\000\053\000\000\000\000\000\
\\050\000\051\000\000\000\052\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\053\000";

val yycheck = "\021\000\
\\003\000\040\000\001\000\011\001\016\001\027\000\009\000\019\001\
\\030\000\008\001\006\001\008\001\011\001\008\001\011\001\024\001\
\\038\000\039\000\001\001\002\001\012\001\060\000\004\001\005\001\
\\006\001\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\\015\001\016\001\030\001\001\001\002\001\059\000\020\001\021\001\
\\062\000\023\001\019\001\046\000\027\001\003\001\009\001\012\001\
\\030\001\015\001\016\001\025\001\010\001\027\001\012\001\013\001\
\\027\001\079\000\080\000\017\001\018\001\019\001\004\001\009\001\
\\022\001\012\001\019\001\019\001\026\001\027\001\028\001\003\001\
\\009\001\031\001\032\001\025\001\027\001\008\001\010\001\027\001\
\\012\001\013\001\027\001\010\000\037\000\017\001\018\001\019\001\
\\255\255\255\255\022\001\005\001\006\001\255\255\026\001\006\001\
\\028\001\255\255\255\255\031\001\032\001\004\001\005\001\006\001\
\\007\001\008\001\020\001\021\001\011\001\023\001\021\001\014\001\
\\023\001\255\255\255\255\029\001\030\001\020\001\021\001\030\001\
\\023\001\024\001\025\001\255\255\027\001\255\255\029\001\030\001\
\\004\001\005\001\006\001\007\001\008\001\255\255\255\255\011\001\
\\255\255\255\255\014\001\255\255\255\255\255\255\255\255\255\255\
\\020\001\021\001\255\255\023\001\024\001\025\001\255\255\027\001\
\\255\255\029\001\030\001\004\001\005\001\006\001\007\001\008\001\
\\255\255\255\255\011\001\255\255\255\255\014\001\255\255\255\255\
\\255\255\255\255\255\255\020\001\021\001\255\255\023\001\024\001\
\\025\001\255\255\027\001\255\255\029\001\030\001\004\001\005\001\
\\255\255\007\001\008\001\255\255\255\255\011\001\255\255\255\255\
\\014\001\255\255\255\255\255\255\255\255\255\255\020\001\021\001\
\\255\255\023\001\024\001\025\001\255\255\027\001\255\255\029\001\
\\004\001\005\001\255\255\007\001\008\001\255\255\255\255\011\001\
\\004\001\005\001\014\001\007\001\008\001\255\255\255\255\011\001\
\\020\001\021\001\014\001\023\001\024\001\025\001\255\255\027\001\
\\020\001\029\001\255\255\255\255\024\001\025\001\255\255\027\001\
\\255\255\029\001\004\001\005\001\255\255\007\001\008\001\255\255\
\\255\255\011\001\004\001\255\255\014\001\007\001\008\001\255\255\
\\255\255\011\001\020\001\255\255\014\001\255\255\024\001\025\001\
\\255\255\027\001\255\255\029\001\255\255\255\255\024\001\025\001\
\\255\255\027\001\004\001\029\001\255\255\007\001\008\001\255\255\
\\255\255\011\001\255\255\255\255\014\001\005\001\006\001\255\255\
\\255\255\255\255\005\001\006\001\255\255\255\255\024\001\025\001\
\\255\255\027\001\255\255\029\001\020\001\021\001\255\255\023\001\
\\255\255\020\001\021\001\027\001\023\001\024\001\030\001\005\001\
\\006\001\255\255\255\255\030\001\005\001\006\001\007\001\255\255\
\\255\255\255\255\255\255\255\255\005\001\006\001\020\001\021\001\
\\255\255\023\001\255\255\020\001\021\001\027\001\023\001\255\255\
\\030\001\005\001\006\001\020\001\021\001\030\001\023\001\255\255\
\\255\255\255\255\014\001\255\255\255\255\030\001\255\255\255\255\
\\020\001\021\001\255\255\023\001\255\255\255\255\255\255\255\255\
\\255\255\255\255\030\001";

val yyact = vector_ 36 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 41 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.UnknownTypes.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : Fasto.UnknownTypes.Prog))
;
(* Rule 2, file Parser.grm, line 44 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.FunDec
val d__3__ = peekVal 0 : Fasto.UnknownTypes.FunDec list
in
( (d__2__) :: (d__3__) ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 3, file Parser.grm, line 45 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.FunDec
in
( (d__2__) :: [] ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 4, file Parser.grm, line 49 *)
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
(* Rule 5, file Parser.grm, line 51 *)
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
(* Rule 6, file Parser.grm, line 54 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Int ) end : Fasto.Type))
;
(* Rule 7, file Parser.grm, line 55 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Bool ) end : Fasto.Type))
;
(* Rule 8, file Parser.grm, line 56 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Char ) end : Fasto.Type))
;
(* Rule 9, file Parser.grm, line 57 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.Type
val d__3__ = peekVal 0 : (int*int)
in
( Array (d__2__) ) end : Fasto.Type))
;
(* Rule 10, file Parser.grm, line 61 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 3 : Fasto.Type
val d__2__ = peekVal 2 : string*(int*int)
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Fasto.Param list
in
( Param (#1 (d__2__), (d__1__)) :: (d__4__) ) end : Fasto.Param list))
;
(* Rule 11, file Parser.grm, line 62 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.Type
val d__2__ = peekVal 0 : string*(int*int)
in
( Param (#1 (d__2__), (d__1__)) :: [] ) end : Fasto.Param list))
;
(* Rule 12, file Parser.grm, line 65 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( Constant (IntVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 13, file Parser.grm, line 66 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 0 : char*(int*int)
in
( Constant (CharVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 14, file Parser.grm, line 67 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( Var (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 15, file Parser.grm, line 68 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( StringLit (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 16, file Parser.grm, line 69 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Constant ((BoolVal true), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 17, file Parser.grm, line 70 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Constant ((BoolVal false), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 18, file Parser.grm, line 72 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__3__ = peekVal 0 : (int*int)
in
( ArrayLit ((d__2__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 19, file Parser.grm, line 73 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Times ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 20, file Parser.grm, line 74 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Divide ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 21, file Parser.grm, line 75 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Plus ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 22, file Parser.grm, line 76 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Minus((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 23, file Parser.grm, line 77 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Equal((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 24, file Parser.grm, line 78 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Less ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 25, file Parser.grm, line 80 *)
val _ = update_ yyact 25
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
(* Rule 26, file Parser.grm, line 82 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), (d__3__), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 27, file Parser.grm, line 84 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), [], #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 28, file Parser.grm, line 87 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.Type
val d__4__ = peekVal 0 : (int*int)
in
( Read ((d__3__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 29, file Parser.grm, line 89 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Write ((d__3__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 30, file Parser.grm, line 91 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 31, file Parser.grm, line 93 *)
val _ = update_ yyact 31
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
(* Rule 32, file Parser.grm, line 95 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Index (#1 (d__1__), (d__3__), (), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 33, file Parser.grm, line 98 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp list
in
( (d__1__) :: (d__3__) ) end : Fasto.UnknownTypes.Exp list))
;
(* Rule 34, file Parser.grm, line 99 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( (d__1__) :: [] ) end : Fasto.UnknownTypes.Exp list))
;
(* Entry Prog *)
val _ = update_ yyact 35 (fn () => raise yyexit (peekVal 0));
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
