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
                                       | _ => err(pos, "INT required")

fun checkRecord (symbol, pos, l) =
    case l of [] => (err(pos, (S.name symbol) ^ " not found in record"); TY.BOTTOM)
            | ((s, ty)::rest) =>
              let
                  val symbolname = S.name symbol
                  val _ = print("checkRecord: " ^ symbolname ^ "\n");
              in
                  if S.name s = symbolname then ty else checkRecord(symbol, pos, rest)
              end

fun checkSimpleVar venv (symbol, pos) =
    let
        val _ = print("SimpleVar: " ^ S.name symbol ^ "\n")
    in
        case S.look(venv, symbol) of SOME(E.VarEntry ty) => {exp=(), ty=(#ty ty)}
                                   | _ => (err(pos, S.name symbol ^ " not declared or not a variable"); {exp=(), ty=TY.BOTTOM})
    end
and checkFieldVar venv tenv (var, symbol, pos) =
    let
        val _ = print("FieldVar: " ^ S.name symbol ^ "\n")
    in
        case var of A.SimpleVar(varname, varpos) =>
                    let
                        val ty = checkSimpleVar venv (varname, varpos)
                    in
                        case (#ty ty) of TY.RECORD (tylist, _) =>
                                         let
                                             val _ = print(S.name varname ^ " is a RECORD" ^ "\n")
                                             val innerType = checkRecord(symbol, pos, tylist)
                                         in
                                             {exp=(), ty=innerType}
                                         end
                                       | _ => (err(pos, S.name varname ^ " is not a RECORD"); {exp=(), ty=TY.BOTTOM})
                    end
                  | A.FieldVar (var', symbol', pos') =>
                    let
                        val _ = print("Nested symbol: " ^ S.name symbol' ^ "\n")
                        val innerType = checkFieldVar venv tenv (var', symbol', pos')
                    in
                        case #ty innerType of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist)}
                                        | _ => (err(pos, "Inner type is not a RECORD"); {exp=(), ty=TY.BOTTOM})
                    end
                  | A.SubscriptVar(var', exp', pos') => checkSubscriptVar venv tenv (var', exp', pos')
    end
and checkSubscriptVar venv tenv (var, exp, pos) =
    case var of A.SimpleVar(varname, varpos) =>
                let
                    val ty = checkSimpleVar venv (varname, varpos)
                in
                    case #ty ty of TY.ARRAY (arrty, _) => {exp=(), ty=arrty}
                                 | _ => (err(pos, S.name varname ^ "is not an ARRAY"); {exp=(), ty=TY.BOTTOM})
                end
              | A.FieldVar (var', symbol', pos') => checkFieldVar venv tenv (var', symbol', pos')
              | A.SubscriptVar (var', exp', pos') =>
                let
                    val innerType = checkSubscriptVar venv tenv (var', exp', pos')
                in
                    case #ty innerType of TY.ARRAY (ty, _) => {exp=(), ty=ty}
                                        | _ => (err(pos, "Expected ARRAY"); {exp=(), ty=TY.BOTTOM})
                end

fun checkSameType ({exp=_, ty=ty1}, {exp=_, ty=ty2}, pos) = doCheckSameType(ty1, ty2, pos)
and doCheckSameType (ty1, ty2, pos) =
    (* Checks whether ty1 and ty2 are compatible *)
    case ty1 of TY.RECORD(_, ty1Unique) =>
                (case ty2 of TY.RECORD(_, ty2Unique) =>
                             if ty1Unique = ty2Unique
                             then ()
                             else err(pos, "RECORD types mismatch")
                           | TY.NIL => ()
                           | TY.BOTTOM => ()
                           | _ => err(pos, "Expected RECORD type."))
              | TY.NIL =>
                (case ty2 of TY.RECORD _ => ()
                           | TY.NIL => ()
                           | TY.BOTTOM => ()
                           | _ => err(pos, "Expected NIL type."))
              | TY.INT =>
                (case ty2 of TY.INT => ()
                           | TY.BOTTOM => ()
                           | _ => err(pos, "Expected INT type."))
              | TY.STRING =>
                (case ty2 of TY.STRING => ()
                           | TY.BOTTOM => ()
                           | _ => err(pos, "Expected STRING type."))
              | TY.BOTTOM => ()
              | TY.ARRAY(_, ty1Unique) =>
                (case ty2 of TY.ARRAY(_, ty2Unique) =>
                             if ty1Unique = ty2Unique
                             then()
                             else err(pos, "ARRAY types mismatch")
                           | TY.BOTTOM => ()
                           | _ => err(pos, "Expected ARRAY type."))
              | TY.UNIT =>
                (case ty2 of TY.UNIT => ()
                           | TY.BOTTOM => ()
                           | _ => err(pos, "Expected RECORD type."))
              | _ => ()
fun transExp (venv: venvType, tenv:tenvType, exp:A.exp) =
    let
        fun trexp(A.NilExp) = {exp=(), ty=TY.NIL}
          | trexp(A.IntExp _) = {exp=(), ty=TY.INT}
          | trexp(A.StringExp _) = {exp=(), ty=TY.STRING}
          | trexp(A.OpExp {left=leftExp, oper=oper, right=rightExp, pos=pos}) =
            if (oper = A.PlusOp orelse
                oper = A.MinusOp orelse
                oper = A.TimesOp orelse
                oper = A.DivideOp orelse
                oper = A.LtOp orelse
                oper = A.LeOp orelse
                oper = A.GtOp orelse
                oper = A.GeOp)
            then
                (checkInt(transExp(venv, tenv, leftExp), pos);
                 checkInt(transExp(venv, tenv, rightExp), pos);
                 {exp=(), ty=TY.INT})
            else (* oper = EqOp orelse oper = NeqOp *)
                (checkSameType(trexp(leftExp), trexp(rightExp), pos);
                 {exp=(), ty=TY.INT})
          | trexp (A.VarExp var) = trvar(var)
          | trexp (A.LetExp {decs, body, pos}) =
            let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
            in
                transExp(venv', tenv', body)
            end

          | trexp (A.SeqExp []) = {exp=(), ty=TY.UNIT}
          | trexp (A.SeqExp [(exp, pos)]) = trexp(exp)
          | trexp (A.SeqExp ((exp:A.exp, pos:Absyn.pos)::left)) = (trexp(exp);trexp(A.SeqExp left))

          | trexp (A.AssignExp {var, exp, pos}) =
            let
              val varTy = trvar (var)
              val expTy = trexp(exp)
              val _ = checkSameType(varTy, expTy, pos)
            in
              {exp=(), ty=TY.UNIT}
            end

          | trexp _ = {exp=(), ty=TY.BOTTOM}
        and trvar (A.SimpleVar(symbol, pos)) = checkSimpleVar venv (symbol, pos)
          | trvar (A.FieldVar(var, symbol, pos)) = checkFieldVar venv tenv (var, symbol, pos)
          | trvar (A.SubscriptVar(var, exp, pos)) =
            let
                val _ = checkInt(trexp(exp), pos)
            in
                checkSubscriptVar venv tenv (var, exp, pos)
            end

    in
        trexp(exp)
    end
and transDecs(venv: venvType, tenv: tenvType, decs: A.dec list) =
    foldl (fn (dec, {venv=venv, tenv=tenv}) => transDec(venv, tenv, dec)) {venv=venv, tenv=tenv} decs
and transDec (venv, tenv, dec) =
    case dec of A.VarDec {name=name, typ=typ, init=init, ...} =>
                let
                    val {exp=_, ty=initType} = transExp(venv, tenv, init)
                in
                    case typ of NONE =>
                                (* If a variable type is not specified, simply take whatever type the
                                   expression returns and enter it into venv as a VarEntry *)
                                {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=initType})}
                              | SOME(symbol, pos) =>
                                (* If a variable type is specified, first check the type of the init exp.
                                   If init exp returns NIL, then check if symbol represents a RECORD.
                                   Otherwise, check if symbol names the type returned by init exp *)
                                let
                                    val symbolTy = S.look(tenv, symbol)
                                in
                                    case symbolTy of SOME(symbolType) =>
                                                     let
                                                         val _ = doCheckSameType(symbolType, initType, pos)
                                                     in
                                                         {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=symbolType})}
                                                     end
                                                   | NONE =>
                                                     (err(pos, "Type " ^ S.name symbol ^ " is not found");
                                                      {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=TY.BOTTOM})})
                                end

                end
              | A.FunctionDec (decs) =>
                let
                    fun transParam {name, typ, pos, escape} =
                        case S.look(tenv, typ) of SOME t => {name=name, ty=t}
                                                | NONE =>
                                                  (err(pos, "Type " ^ S.name typ ^ " not found"); {name=name, ty=TY.BOTTOM})
                    fun enterParam ({name, ty}, venv) = S.enter(venv, name, E.VarEntry{ty=ty})
                    fun checkFunctionDec ({name, params, body: A.exp, pos, result=SOME(rt, _)}, {venv: venvType, tenv: tenvType}) =
                        let
                            val result_ty = case S.look(tenv, rt) of SOME(ty) => ty
                                                                   | NONE => (err(pos, "Type " ^ S.name rt ^ " not found"); TY.BOTTOM)
                            val params' = map transParam params
                            val venv' = S.enter(venv, name, E.FunEntry{formals=map #ty params', result=result_ty})
                            val venv'' = foldl enterParam venv' params'
                            val actual_rt = transExp(venv'', tenv, body);
                            val _ = doCheckSameType(result_ty, #ty actual_rt, pos)
                        in
                            {venv=venv', tenv=tenv}
                        end
                      | checkFunctionDec ({name, params, body: A.exp, pos, result=NONE}, {venv: venvType, tenv: tenvType}) =
                        {venv=venv, tenv=tenv}
                in
                    foldl checkFunctionDec {venv=venv, tenv=tenv} decs
                end
              | A.TypeDec decs =>
                let
                    fun checkTypeDec ({name, ty, pos}, {venv=venv, tenv=tenv}): {venv: venvType, tenv: tenvType} =
                        case ty of A.NameTy(symbol, _) =>
                                   (case S.look(tenv, symbol) of SOME(ty) => {venv=venv, tenv=S.enter(tenv, name, ty)}
                                                              | NONE =>
                                                                (err(pos, "Type " ^ S.name symbol ^ " not found");
                                                                 {venv=venv, tenv=S.enter(tenv, name, TY.BOTTOM)}))
                                 | A.ArrayTy(symbol, _) =>
                                   (case S.look(tenv, symbol) of SOME(ty) => {venv=venv, tenv=S.enter(tenv, name, TY.ARRAY(ty, ref ()))}
                                                               | NONE =>
                                                                 (err(pos, "Type " ^ S.name symbol ^ " not found");
                                                                  {venv=venv, tenv=S.enter(tenv, name, TY.ARRAY(TY.BOTTOM, ref()))}))
                                 | A.RecordTy fields =>
                                   let
                                       fun checkField {name, escape, typ, pos} =
                                           case S.look(tenv, typ) of SOME(ty) => (name, ty)
                                                                   | NONE =>
                                                                     (err(pos, "Type " ^ S.name typ ^ " not found");
                                                                      (name, TY.BOTTOM))
                                   in
                                       {venv=venv, tenv=S.enter(tenv, name, TY.RECORD(map checkField fields, ref ()))}
                                   end
                in
                    foldl checkTypeDec {venv=venv, tenv=tenv} decs
                end
fun transTy (tenv:venvType, ty:A.ty):TY.ty = TY.NIL

fun transProg (AST_expression:A.exp) =
    let
        val _ = transExp(E.base_venv, E.base_tenv, AST_expression)
    in
        ()
    end

end