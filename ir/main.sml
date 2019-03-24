structure Main =
struct
fun typeCheck(fileName : string) = Semant.transProg (Parse.parse (fileName))
fun debug(fileName : string) =
    let
        val ast = Parse.parse(fileName)
        val _ = PrintAbsyn.print(TextIO.stdOut, ast)
    in
        Semant.transProg(Parse.parse (fileName))
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
