structure A = Absyn
structure TY = Types
structure S = Symbol
structure E = Env
structure TR = Translate

structure Semant =
struct

fun transExp (venv, tenv, exp) = ()

fun transProg (AST_expression : A.exp) =
  let
    val _ = transExp(E.base_venv, E.base_tenv, AST_expression)
  in
    ()
  end

end
