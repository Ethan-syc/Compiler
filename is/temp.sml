(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
type temp = int
val temps = ref 100
fun newtemp() = let val t = !temps in temps := t+1; t end

structure Table = IntMapTable(type key = int
			      fun getInt n = n)

fun makestring t = "t" ^ Int.toString t

type label = Symbol.symbol
val labs = ref 0
fun reset () = (temps := 128; labs := 0)
fun resetzero () = (temps := 100; labs := 0)

local
    structure F = Format
    fun postinc x =
        let val i = !x
        in x := i+1;
           i
        end
in
fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
val namedlabel = Symbol.symbol
end

end
