# Usage:
`CM.make("sources.cm");`

`Main.compile("./args.tig");`

If no Compiler error(s) produced above, then the MIPS assembly code is located at `./out/args.tig.s`

# Extra Credits:
For the Spilling I believe that you are right, for some cases our spilling work but for some cases it freeze so I don’t think we can get EC for spilling. Here are the rest EC:
1. More arguments: We supported more than 4 arguments for function by pushing into frame instead of put in register. A test case is provided (args.tig) in the workingCompiler dir.
2. Dataflow functor: Drew mentioned we can have extra credit for implementing functor for dataflow analysis so that it could be reuse for later phase optimizations like reaching definition and available expression. Now adding them just require to provide functions for gen and kill and how to calculate In and Out. We didn’t have time to use the functor except once for liveness analysis, so I don’t know if we can get some EC for implementing the functor.
3. Fully Functional Type Checker: already got EC in Semant Phase
# Bug Fix:
## Lexer:
Fixed in tiger.lex

(-10): Nested comments not implemented (HINT: What happens when you encounter a comment inside a comment)

## Parser:
Fixed in tiger.grm

(-5): Declaration list with zero elements not implemented.

(-8): Let expressions incorrectly implemented, they do not require parentheses around the expression sequence. This has the additional effect of not allowing let expressions without any expressions to execute (for instance just let in end does not parse correctly)

## IR/IS
Fixed in translate.sml and codegen.sml and runtimele.s

malloc before initArray (-1)

nil checking of records not present (-4) Add MIPS code in runtimele.s

bounds checking on array not implemented (-4) Add MIPS code in runtimele.s

Wrong static link calculation (-3)

No output on test5, (-5) This is caused by our debugging function, since it will keep trying to resolve the pending type we added for fully functional type checker and that will cause infinite recursion

failed test27 (-3)

return value for some test cases not loaded (-4)
