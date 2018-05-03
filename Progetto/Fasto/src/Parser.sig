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
type t__24__ = int*(int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = string*(int*int)
type t__32__ = (int*int)
type t__33__ = (int*int)
type t__34__ = (int*int)
type t__35__ = (int*int)
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
  | NOT of t__23__
  | NUM of t__24__
  | OR of t__25__
  | PLUS of t__26__
  | RBRACKET of t__27__
  | RCURLY of t__28__
  | READ of t__29__
  | RPAR of t__30__
  | STRINGLIT of t__31__
  | THEN of t__32__
  | TIMES of t__33__
  | TRUE of t__34__
  | WRITE of t__35__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Fasto.UnknownTypes.Prog;
