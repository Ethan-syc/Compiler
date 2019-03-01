structure A = Absyn
structure TY = Types
structure S = Symbol
structure E = Env
structure TR = Translate

structure Semant =
struct

type venvType = E.enventry S.table
type tenvType = E.ty S.table
type expty = {exp:TR.exp, ty:TY.ty}

fun err(pos,message) = ErrorMsg.error pos message

fun checkInt ({exp,ty},pos) = case ty of TY.INT => ()
                                      |  _      => err(pos, "INT required")

fun checkSameType ({exp=_,ty=TY.RECORD (_, leftUnique)}, {exp=_,ty=TY.RECORD (_, rightUnique)}, pos) =
      if leftUnique = rightUnique
      then ()
      else err(pos, "RECORD TYPE mismatch")
  | checkSameType ({exp=_,ty=TY.NIL}, {exp=_,ty=TY.RECORD (_, leftUnique)}, pos) = ()
  | checkSameType ({exp=_,ty=TY.RECORD (_, leftUnique)}, {exp=_,ty=TY.NIL}, pos) = ()
  | checkSameType ({exp=_,ty=TY.INT}, {exp=_,ty=TY.INT}, pos) = ()
  | checkSameType ({exp=_,ty=TY.STRING}, {exp=_,ty=TY.STRING}, pos) = ()
  | checkSameType ({exp=_,ty=TY.ARRAY (_, leftUnique)}, {exp=_,ty=TY.ARRAY (_, rightUnique)}, pos) =
      if leftUnique = rightUnique
      then ()
      else err(pos, "ARRAY TYPE mismatch")
  | checkSameType (_, _, pos) = err(pos, "TYPE mismatch")

fun transVar (venv:venvType, tenv:tenvType, var:A.var):expty = {exp=(), ty=TY.NIL}

fun transExp (venv: venvType, tenv:tenvType, exp:A.exp) =
  let
    fun trexp(A.NilExp) = {exp=(), ty=TY.NIL}
      | trexp(A.IntExp _) = {exp=(), ty=TY.INT}
      | trexp(A.StringExp _) = {exp=(), ty=TY.STRING}
      | trexp(A.OpExp {left=leftExp, oper=oper, right=rightExp, pos=pos}) =
        if (oper = A.PlusOp orelse
            oper = A.MinusOp orelse
            oper=A.TimesOp orelse
            oper=A.DivideOp orelse
            oper=A.LtOp orelse
            oper=A.LeOp orelse
            oper=A.GtOp orelse
            oper=A.GeOp)
        then
          (checkInt(transExp(venv, tenv, leftExp), pos);
           checkInt(transExp(venv, tenv, rightExp), pos);
           {exp=(), ty=TY.INT})
        else (* oper = EqOp orelse oper = NeqOp *)
          (checkSameType(transExp(venv, tenv, leftExp), transExp(venv, tenv, rightExp), pos);
           {exp=(), ty=TY.INT})
  in
    trexp(exp)
  end

fun transDec (venv:venvType, tenv:tenvType, dec:A.dec):{venv:venvType, tenv:tenvType} = {venv=venv, tenv=tenv}

fun tranTy (tenv:venvType, ty:A.ty):TY.ty = TY.NIL

fun transProg (AST_expression:A.exp) =
  let
    val _ = transExp(E.base_venv, E.base_tenv, AST_expression)
  in
    ()
  end

end
