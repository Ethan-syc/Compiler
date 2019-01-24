datatype expr =
         NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr
         | TIMES of expr * expr
         | DIV of expr * expr
         | F of expr list * (int list -> int)
fun eval (NUM n) = n
  | eval (PLUS(e1,e2)) = eval(e1) + eval(e2)
  | eval (MINUS(e1,e2)) = eval(e1) - eval(e2)
  | eval (TIMES(e1,e2)) = eval(e1) * eval(e2)
  | eval (DIV(e1,e2)) = eval(e1) div eval(e2)
  | eval (F([],g)) = 0
  | eval (F(expList,g)) = g (map eval expList)

fun flatten [[]] = []
  | flatten aListList = foldr op @ [] aListList

fun foldmap f [] = []
  | foldmap f aList =
    foldr (fn (a, list) => (f a)::list) [] aList

fun foldfilter f [] = []
  | foldfilter f beforeList =
    foldr (fn (a, list) => if f a then a::list else list) [] beforeList

fun count f [] = 0
  | count f aList =
    foldr (fn (a, b) => if f a then b+1 else b) 0 aList

fun mapPartial f [] = []
  | mapPartial f (a::l) =
    foldr (fn (a, list) =>
              case f a of
                  NONE => list
                | _ => (valOf (f a))::list) [] (a::l)
