type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
val currString = ref ""
val stringStartPos = ref 0
val stringOpenState = ref 0

fun err(p1,p2) = ErrorMsg.error p1 p2
fun newLine(yypos) =
    let
        val _ = lineNum := !lineNum+1;
        (* val _ = print("[#line#] Incrementing line to " ^ Int.toString(!lineNum) ^ "\n") *)
        val _ = linePos := yypos :: !linePos;
    in
        ()
    end
fun pr i =
    let
        val _ = print(Int.toString(i) ^ " ")
    in
        ()
    end
fun gotoState (sourceState, destState) =
    let
        (* val _ = print("[#state#] From " ^ sourceState ^ " to " ^ destState ^ "\n") *)
    in
        ()
    end
fun eof() =
    let
        val pos = hd(!linePos)
        (* val _ = print("[#linePos#] ")
        val _ = map pr (!linePos)
        val _ = print("\n") *)
        val _ = lineNum := 1
        val _ = linePos := [1]
    in
        if (!commentDepth <> 0) then err(pos, "unmatched comment depth at EOF") else ();
        if (!stringOpenState <> 0) then err(pos, "string is still open at EOF") else ();
        Tokens.EOF(pos,pos)
    end

%%

digit=[0-9];
letter=[a-zA-Z];
control=[@A-Z[\]^_];
formattingChar=[ \012\009];
whitespace=[ \012\009\n];
%s COMMENT STRING FORMATTING;
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%%

<INITIAL>"type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>"of" => (Tokens.OF(yypos, yypos+2));
<INITIAL>"end" => (Tokens.END(yypos, yypos+3));
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
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));
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
<INITIAL>[a-zA-Z]([a-zA-Z0-9_])* => (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL>\n => (newLine(yypos); continue());
<INITIAL>{whitespace} => (continue());

<INITIAL>"/*" =>(commentDepth := !commentDepth + 1; gotoState("INITIAL", "COMMENT"); YYBEGIN COMMENT; continue());
<INITIAL>"*/" =>(err(yypos, "unexpected comment terminator"); continue());
<COMMENT>"*/" =>(commentDepth := !commentDepth - 1;
                 if !commentDepth = 0 then
                     let
                         val _ = gotoState("COMMENT", "INITIAL")
                     in
                         YYBEGIN INITIAL
                     end
                 else ();
                 continue());
<COMMENT>\n => (newLine(yypos); continue());
<COMMENT>. => (continue());

<INITIAL>\034 => (gotoState("INITIAL", "STRING"); YYBEGIN STRING; currString := ""; stringStartPos := yypos; stringOpenState := 1; continue());
<STRING>\092\110 => (currString := !currString ^ "\n"; continue());
<STRING>\092\116 => (currString := !currString ^ "\t"; continue());
<STRING>\092\094{control} => (currString := !currString ^ Char.toString (valOf(Char.fromString yytext)); continue());
<STRING>\\{digit}{digit}{digit} => (currString := !currString ^ Char.toString (valOf(Char.fromString yytext)); continue());
<STRING>\092\034 => (currString := !currString ^ "\""; continue());
<STRING>\092\092 => (currString := !currString ^ "\\"; continue());
<STRING>\010 => (err(yypos, "EOL while scanning string literal, string literal can't contain new line character"); newLine(yypos); continue());
<STRING>\009 => (err(yypos, "EOL while scanning string literal, string literal can't contain tab character"); continue());
<STRING>\092 => (gotoState("STRING", "FORMATTING"); YYBEGIN FORMATTING; continue());
<STRING>\034 => (gotoState("STRING", "INITIAL"); YYBEGIN INITIAL; stringOpenState := 0; Tokens.STRING(!currString, !stringStartPos, yypos));
<STRING>. => (currString := !currString ^ yytext; continue());

<FORMATTING>\092 => (gotoState("FORMATTING", "STRING"); YYBEGIN STRING; continue());
<FORMATTING>{formattingChar} => (continue());
<FORMATTING>\n => (newLine(yypos); continue());
<FORMATTING>. => (err(yypos, "expecting only white space in formatting string block"); continue());

. => (err(yypos, "no rule matched, invalid token"); continue());
\n	=> (newLine(yypos); continue());
