local open Obj Lexing in


open Parser;        (* The token type is defined in Parser.sig *)

val intOfString = valOf o Int.fromString;


fun action_11 lexbuf = (
 raise Fail "illegal symbol" )
and action_10 lexbuf = (
 EOF )
and action_9 lexbuf = (
 UMINUS )
and action_8 lexbuf = (
 RPAREN )
and action_7 lexbuf = (
 LPAREN )
and action_6 lexbuf = (
 DIV )
and action_5 lexbuf = (
 TIMES )
and action_4 lexbuf = (
 MINUS )
and action_3 lexbuf = (
 PLUS )
and action_2 lexbuf = (
 INT(intOfString (getLexeme lexbuf)) )
and action_1 lexbuf = (
 EOL )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_11 lexbuf
 else case currChar of
    #"\t" => action_0 lexbuf
 |  #" " => action_0 lexbuf
 |  #"~" => action_9 lexbuf
 |  #"/" => action_6 lexbuf
 |  #"-" => action_4 lexbuf
 |  #"+" => action_3 lexbuf
 |  #"*" => action_5 lexbuf
 |  #")" => action_8 lexbuf
 |  #"(" => action_7 lexbuf
 |  #"\n" => action_1 lexbuf
 |  #"\^@" => action_10 lexbuf
 |  _ => action_11 lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_13 lexbuf
 else backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_13 lexbuf
 else backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
