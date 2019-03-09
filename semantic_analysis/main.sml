structure Main =
struct
  fun typeCheck(fileName : string) = Semant.transProg (Parse.parse (fileName))
  fun debug(fileName : string) = Semant.transExp(Env.base_venv, Env.base_tenv, Parse.parse (fileName))
end
