structure FormatAssem =
struct

structure A = Assem

fun format saytemp =
    let
        fun speak(assem,dst,src,jump) =
            let val saylab = Symbol.name
        	fun f(#"`":: #"s":: i::rest) =
        	    (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
        	  | f( #"`":: #"d":: i:: rest) =
        	    (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
        	  | f( #"`":: #"j":: i:: rest) =
        	    (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
        	  | f( #"`":: #"`":: rest) = #"`" :: f rest
        	  | f( #"`":: _ :: rest) = ErrorMsg.impossible "bad Assem format"
        	  | f(c :: rest) = (c :: f rest)
        	  | f nil = nil
            in implode(f(explode assem))
            end
    in fn A.OPER{assem,dst,src,jump} => assem(MipsFrame.regToString)
     | A.LABEL{assem,...} => assem
     | A.MOVE{assem,dst,src} => assem(MipsFrame.regToString)
    end


end
