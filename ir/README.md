# Frame Analysis and Intermediate Representation
We mostly followed the book and had some minor change on few function signature.<br>
To test, compile the code use
`CM.make("sources.cm"); `
then call `Main.semant("test.tig");`, it will print out the semantic error(s) it finds and do nothing if no error is found.<br>
To print out the frag list, call `Main.debug("test.tig");`

## Worth Mentioning
### 1. External Call
We used some external call in our IR to reduce code size like checkNil, boundaryCheck...
### 2. String compare functions
We didn't provided string compare functions in IR because we thing if the programmer want to compare two string, they should have the ability to compare the address of the two string and if they want to compare the character, they can use other library function to do it.
### 3. Upgraded functional way for checking mutually recursive types instead of side-effecting NAME type
We upgraded out functional way after talking to Professor Drew Hilton, now we use a type PENDING of unit -> ty, this is brilliant.
