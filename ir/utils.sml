structure Utils =
struct

structure T = Tree
(* A specialization of List.exists for bool lists *)
fun anyOf l = List.exists (fn(x) => x) l

fun arrayMul (value, 0) = []
  | arrayMul (value, numRepeats) = value::arrayMul (value, numRepeats - 1)

fun range (startvalue, endvalue) =
    if startvalue >= endvalue then [] else (startvalue::range(startvalue + 1, endvalue))

fun debug msg = if true then print("DEBUG: " ^ msg ^ "\n") else ()

fun seq [] =
    (debug("Compiler error: seq called on empty list");
     T.LABEL(Temp.newlabel()))
  | seq [stm1] = stm1
  | seq (stm1::stm2::[]) = T.SEQ(stm1, stm2)
  | seq (stm1::rest) = T.SEQ(stm1, seq(rest))

end
