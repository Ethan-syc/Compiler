# Parsing & Abstract Syntax

## Parsing
We followed the Tiger Language Reference Manual located in the appendix section of the book to write our grammar, and it has 2 shift-reduce conflicts which is not harmful.

* 1.  error:  state 31: shift/reduce conflict (shift FUNCTION, reduce by rule 25)
state 31:
	fundecs : fundec . fundecs
	fundecs : fundec .  (reduce by rule 25)
* 2.  error:  state 33: shift/reduce conflict (shift TYPE, reduce by rule 14)
state 33:
	tydecs : tydec . tydecs
	tydecs : tydec .  (reduce by rule 14)

These 2 shift-reduce conflicts are happending because we want to parse multually recursive function and type declaration. And in these cases we can just go with shift which is the ML-Yacc's default behaviour for any shift-reduce conflict.

## Abstract Syntax
We first had a deep look into the provided abstract syntax file and all the datatypes in it, and then we allocate proper datatype to our non-terminals and provided the semantic actions accordingly.
