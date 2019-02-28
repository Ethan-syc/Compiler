structure Main =
struct
  fun typeCheck(fileName : string) = Semant.transProg (Parse.parse (fileName))
end
