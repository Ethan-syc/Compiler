# Lexical Analysis
### 1. Comments Handling
We have a state COMMENT for comments handling. If the lexer find a /* in INITIAL state, it will goes to COMMENT state and we use a reference commentDepth for nested comments, +1 when every /* is encountered, and -1 when every */ is encountered.
### 2. Strings Handling
We have a state STRING for string handling. If the lexer find a " in INITIAL state, it will goes to STRING state and we use a reference currString for concatenating string, a reference stringStartPos for keep tracking the string start location and a reference stringOpenState for indicating if the string is properly close. And we will lex the string use the escape rules mentioned in the Tiger Language Reference Manual located in the appendix section of the book.
We also have another state FORMATTING for formatting characters in string. Lexer will goes into this state when encounter a \ in STRING state.
It is worth to mention that we use ascii code for some character and control sequence for clarity. It maybe a little hard to understand something like 
`<STRING>\092\110 => (currString := !currString ^ "\n"; continue());`
But we found it is more clear and easy to debug since we don't escape anything in the regular expression.
### 3. Error Handling
Lexer will produce error message in the following situation
* Encounter a */ in INITIAL state
* Encounter a new line character \n in STRING state
* Encounter a tab character \t in STRING state
* Encounter anything else than formatting characters in FORMATTING state
* Encounter a token that no rule before can match
* Have unclosed comment or string when encounter a EOF
### 4. EOF Handling
We add some sml code to check if there are any unclosed comment or string before return Tokens.EOF.
### 5. Extra Feature
We add some auxiliary sml code to help debug and understand the lexing process like state changing and printing the line number.
To use them, just uncomment line 15, 28, 35-37.
example result when debug function enabled:
```
- Parse.parse("../tiger/testcases/test1.tig");
[#state#] From INITIAL to COMMENT
[#state#] From COMMENT to INITIAL
[#line#] Incrementing line to 2
LET   44
[#line#] Incrementing line to 3
TYPE   49
ID(arrtype)     55
EQ   63
ARRAY   65
OF   71
ID(int)     74
[#line#] Incrementing line to 4
VAR   79
ID(arr1)     83
COLON   87
ID(arrtype)     88
ASSIGN   96
ID(arrtype)     99
LBRACK   107
INT(10)   108
RBRACK   110
OF   112
INT(0)   115
[#line#] Incrementing line to 5
IN   117
[#line#] Incrementing line to 6
ID(arr1)     121
[#line#] Incrementing line to 7
END   126
[#line#] Incrementing line to 8
[#linePos#] 129 125 119 116 77 47 43 1
EOF   129
```

