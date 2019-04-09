structure Temp : TEMP =
struct
    type temp = int
    val compare=Int.compare
    val labelCount = ref 0
    val labs = ref 0
    val temps = ref 100
    structure Table = IntMapTable(type key = int
    			      fun getInt n = n)
    fun reset () =
	let val () = temps := 128
	    val () = labelCount := 0
	in
	    ()
	end
  fun resetzero () = (temps := 100; labs := 0)


    fun newtemp() =
	let val t  = !temps
	    val () = temps := t+1
	in
	    t
	end
    fun makestring t = "t" ^ Int.toString t

    type label = Symbol.symbol

    structure TempOrd =
    struct
      type ord_key = temp
      val compare = compare
    end

    structure Set = SplaySetFn(TempOrd)
    structure Map = SplayMapFn(TempOrd)

    fun newlabel() =
	let val x  = !labelCount
	    val () = labelCount := x +1
      val name = "L" ^ Int.toString (x)
	in
	    Symbol.symbol name
	end
    val namedlabel = Symbol.symbol


end
