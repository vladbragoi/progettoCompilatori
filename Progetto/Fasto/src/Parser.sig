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

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Fasto.UnknownTypes.Prog;
