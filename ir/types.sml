structure Types =
struct

type unique = unit ref (* a unit ref does not equal to another unit ref *)

datatype ty =
         RECORD of (Symbol.symbol * ty) list * unique
         | NIL
         | INT
         | STRING
         | BOTTOM
         | ARRAY of ty * unique
	 (* | NAME of Symbol.symbol * ty option ref *)
         | PENDING of unit -> ty
         | UNIT

fun join [] = ""
  | join (s::[]) = s
  | join (s::rest) = s ^ "," ^ join(rest)

fun fieldToString (symbol, ty) = Symbol.name symbol ^ ": " ^ typeToString ty
and typeToString (RECORD(fields, _)) = "RECORD {" ^ (join (map fieldToString fields)) ^ "}"
  | typeToString (NIL) = "NIL"
  | typeToString (INT) = "INT"
  | typeToString (STRING) = "STRING"
  | typeToString (BOTTOM) = "<ERROR: UNKNOWN>"
  | typeToString (ARRAY(arrType, _)) = "ARRAY of " ^ typeToString(arrType)
  | typeToString (UNIT) = "<NO VALUE>"
  | typeToString (PENDING func) = "<PEDNING>"
end
