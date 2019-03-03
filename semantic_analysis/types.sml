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
	 | NAME of Symbol.symbol * ty option ref
       | UNIT
end
