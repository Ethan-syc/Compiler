structure Utils =
struct

structure T = Tree
(* A specialization of List.exists for bool lists *)
fun anyOf l = List.exists (fn(x) => x) l

fun inList (l, e: string) = List.exists (fn(x) => (x = e)) l

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

fun toLower s =
    implode (map Char.toLower (explode s))

fun binop T.PLUS = "PLUS"
  | binop T.MINUS = "MINUS"
  | binop T.MUL = "MUL"
  | binop T.DIV = "DIV"
  | binop T.AND = "AND"
  | binop T.OR = "OR"
  | binop T.LSHIFT = "LSHIFT"
  | binop T.RSHIFT = "RSHIFT"
  | binop T.ARSHIFT = "ARSHIFT"
  | binop T.XOR = "XOR"

fun relop T.EQ = "EQ"
  | relop T.NE = "NE"
  | relop T.LT = "LT"
  | relop T.GT = "GT"
  | relop T.LE = "LE"
  | relop T.GE = "GE"
  | relop T.ULT = "ULT"
  | relop T.ULE = "ULE"
  | relop T.UGT = "UGT"
  | relop T.UGE = "UGE"

end
