structure Utils =
struct

(* Logging *)
exception Log
val DEBUG = 1
val VERBOSE = 2
val INFO = 3
val WARNING = 4
val ERROR = 5
val NOLOG = 6
(* todo: change this to disable logging *)
val loglevel = DEBUG

fun loglevelToName 1 = "DEBUG"
  | loglevelToName 2 = "VERBOSE"
  | loglevelToName 3 = "INFO"
  | loglevelToName 4 = "WARNING"
  | loglevelToName 5 = "ERROR"
  | loglevelToName _ = raise Log

fun log level msg =
    if level >= loglevel then
        let val date = Date.fromTimeLocal(Time.now())
            val levelname = loglevelToName level
        in
            print((Date.fmt "%c" date) ^ " " ^ levelname ^ ": " ^ msg ^ "\n")
        end
    else ()

val debug = log DEBUG
val verbose = log VERBOSE
val info = log INFO
val warning = log WARNING
val error = log ERROR

structure T = Tree
(* A specialization of List.exists for bool lists *)
fun anyOf l = List.exists (fn(x) => x) l

fun inList (l, e: string) = List.exists (fn(x) => (x = e)) l

fun arrayMul (value, 0) = []
  | arrayMul (value, numRepeats) = value::arrayMul (value, numRepeats - 1)

fun range (startvalue, endvalue) =
    if startvalue >= endvalue then [] else (startvalue::range(startvalue + 1, endvalue))

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
