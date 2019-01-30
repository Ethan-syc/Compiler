type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
val currString = ref ""
val stringStartPos = ref 0
fun err(p1,p2) = ErrorMsg.error p1
fun newLine(yypos) =
  let
  lineNum := !lineNum+1;
  linePos := yypos :: !linePos;
  in
  ()
  end
fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
digit=[0-9]
letter=[a-zA-Z]
control=[@A-Z[\]^_]
formattingChar=[ \012\009] (*space, formfeed, tab*)
%s COMMENT STRING FORMATTING;
%%


<INITIAL>"type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>"of" => (Tokens.OF(yypos, yypos+2));
<INITIAL><INITIAL>"end" => (Tokens.END(yypos, yypos+3));
<INITIAL>"in" => (Tokens.IN(yypos, yypos+2));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL>"let" => (Tokens.LET(yypos, yypos+3));
<INITIAL>"do" => (Tokens.DO(yypos, yypos+2));
<INITIAL>"to" => (Tokens.TO(yypos, yypos+2));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos+3));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL>"if" => (Tokens.IF(yypos, yypos+2));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos+3));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DEVIDE(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"." => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL>"," => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>{digit}+ => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos+size yytext));
<INITIAL>{letter}[{letter}{digit}_]* => (Tokens.ID(yytext, yypos, yypos+size yytext));

<INITIAL>"/*" =>(commentDepth:=!commentDepth+1;yybegin COMMENT; continue());
<COMMENT>"*/" =>(commentDepth:=!commentDepth-1;
  if commentDepth = 0 then yybegin INITIAL
  else if commentDepth < 0 then err(yypos, "unmatched comment terminator")
  else ();
  continue());
<COMMENT>. => (continue();)
<INITIAL>""" => (yybegin STRING; stringStartPos:= yypos; continue());
<STRING>"\n" => (currString:=!currString^"\n"; continue());
<STRING>"\t" => (currString:=!currString^"\t"; continue());
<STRING>\\^{control} => (currString:=!currString^(valOf(Char.fromString yytext)); continue());
<STRING>\\{digit}{digit}{digit} => (currString:=!currString^(valOf(Char.fromString yytext)); continue());
<STRING>"\"" => (currString:=!currString^"\""; continue());
<STRING>"\\" => (currString:=!currString^"\\"; continue());

<STRING>"\" => (yybegin FORMATTING; continue());
<FORMATTING>"\" => (yybegin STRING; continue());
<FORMATTING>{formattingChar} => (continue());
<FORMATTING>\n => (newLine(yypos); continue());
<FORMATTING>.=> (err(yypos, "expecting white space in formatting string block"))
<STRING>\034 => (yybegin INITIAL; Tokens.STRING(!currString, !stringStartPos, yypos));
<STRING>\n	=> (err(yypos, "EOL while scanning string literal"))
<STRING>\t	=> (err(yypos, "EOL while scanning string literal"))
. => (err(yypos, "invalid token"))
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
