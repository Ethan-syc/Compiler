structure Utils =
struct

structure T = Tree
(* A specialization of List.exists for bool lists *)
fun anyOf l = List.exists (fn(x) => x) l
fun allOf l = not(List.exists (fn(x) => not x) l)

fun min f l =
    let val vals = map f l
        fun helper (fx, x, (currfx, currx)) =
            if fx < currfx then (fx, x) else (currfx, currx)
        val min = ListPair.foldl helper (hd vals, hd l) (vals, l)
    in
        #2 min
    end

fun inList (l, e) = List.exists (fn(x) => (x = e)) l

fun arrayMul (value, 0) = []
  | arrayMul (value, numRepeats) = value::arrayMul (value, numRepeats - 1)

fun range (startvalue, endvalue) =
    if startvalue >= endvalue then [] else (startvalue::range(startvalue + 1, endvalue))

fun seq [] =
    (Log.debug("Compiler error: seq called on empty list");
     T.LABEL(Temp.newlabel()))
  | seq [stm1] = stm1
  | seq (stm1::stm2::[]) = T.SEQ(stm1, stm2)
  | seq (stm1::rest) = T.SEQ(stm1, seq(rest))

fun join s ([]) = ""
  | join s ([e]) = e
  | join s (e::rest) = e ^ s ^ (join s rest)

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

fun i2s i =
    if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

end
