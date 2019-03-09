structure Main =
struct
  fun typeCheck(fileName : string) = Semant.transProg (Parse.parse (fileName))
  fun debug(fileName : string) =
  let
   val ast = Parse.parse(fileName)
   val _ = PrintAbsyn.print(TextIO.stdOut, ast)
  in
    Semant.transExp(Env.base_venv, Env.base_tenv, Parse.parse (fileName))
  end

end
