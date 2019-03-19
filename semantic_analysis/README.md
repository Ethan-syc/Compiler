# Semantic Analysis (type-checker)
We mostly followed the book, but we only implemented the transProg transExp and transDecs, and didn't use the signature for transTy and transVar, but we achieved similar functionality use other functions.<br>
To test, compile the code use 
`CM.make("sources.cm"); `
then call `Main.typeCheck("test.tig");`
it will print out the semantic error(s) it finds and do nothing if no error is found.
## Worth Mentioning 
### 1. Functional way for checking mutually recursive types instead of side-effecting NAME type
We replace the NAME type of a PENDING type in types.sml (`PENDING of (ty Symbol.table * int) -> ty`).
When we look at a new type dec, we first look up the right side of the type dec, if we can't find its/their(for fields in record) symbol in tenv, we add its type as PENDING, and the function we pass with PENDING is actualTy(symbol), this actualTy function takes a symbol and returns a function that has the signature (`ty Symbol.table * int) -> ty`), and later we can use the provided tenv to look up the symbol. Also we will resolve all the PENDING ty to make sure they can be correctly resolved for each mutually recursive type decs group that does not contain any type dec cycle.
### 2. Stack for checking BREAK logic
We use sml list as a stack to check for BREAK logic, by using this stack we can make sure:
1. One for/while scope can have at most 1 BREAK
2. Any BREAK must be inside a for/while scope
### 3. DEBUG info
There is a function called debug inside the semant.sml<br>
`fun debug(message) = if false then print("DEBUG: " ^ message) else ()`
change the false to true, and our type checker will print out a lot of useful information like "resolving pending.." or "check on FieldVar.."
