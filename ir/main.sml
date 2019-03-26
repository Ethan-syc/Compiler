structure Main =
struct
fun semant(fileName : string) = Semant.transProg (Parse.parse (fileName))
fun debug(fileName : string) =
    let
        val ast = Parse.parse(fileName)
        val _ = PrintAbsyn.print(TextIO.stdOut, ast)
        val _ = Translate.frags := []
        val frags = Semant.transProg(Parse.parse (fileName))
        val fragNum = ref 0
        fun printFrag (f) =
            (Utils.debug("Printing frag " ^ Int.toString(!fragNum));
             fragNum := 1 + !fragNum;
             MipsFrame.printFrag(TextIO.stdOut, f))
    in
        map printFrag frags
    end
fun runTests(i) =
    if i > 49
    then ()
    else
        let
            val _ = print("========== Running test " ^ Int.toString i ^ " ===========\n")
            val fileName = "../tiger/testcases/test" ^ Int.toString i ^ ".tig"
            val _ = debug(fileName)
            val input = TextIO.inputLine TextIO.stdIn
        in
            case input of SOME("q\n") => ()
                       | _ => runTests(i + 1)
        end
end
