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

fun errAndBottom(pos, message) = (err(pos, message); {exp=(), ty=TY.BOTTOM})

fun checkIsLoopVariable (var, venv, pos) =
  case var of A.SimpleVar(varname, varpos) =>
            (case (S.look(venv, varname)) of SOME(E.VarEntry {ty, loopVar}) => loopVar
                                          | _ => false)
            | _ => false

(* check if the record has the symbol field, return the symbol type, return BOTTOM if not found *)
fun checkRecord (symbol, pos, l) =
    case l of [] => (err(pos, (S.name symbol) ^ " not found in record"); TY.BOTTOM)
            | ((s, ty)::rest) =>
              let
                  val symbolname = S.name symbol
              in
                  if S.name s = symbolname then ty else checkRecord(symbol, pos, rest)
              end

(* check the type of the provided simpleVar, return expty *)
fun checkSimpleVar venv (symbol, pos) =
    let
        val _ = print("SimpleVar: " ^ S.name symbol ^ "\n")
    in
        case S.look(venv, symbol) of SOME(E.VarEntry ty) => {exp=(), ty=(#ty ty)}
                                   | _ => (err(pos, S.name symbol ^ " not declared or not a variable"); {exp=(), ty=TY.BOTTOM})
    end

(* check the type of the provided fieldVar, return expty *)
and checkFieldVar venv tenv (var, symbol, pos) =
    let
        val _ = print("FieldVar: " ^ S.name symbol ^ "\n")
    in
        (* the most simple case, var is simpleVar, type of simpleVar should be TY.RECORD if correct *)
        case var of A.SimpleVar(varname, varpos) =>
                    let
                        val ty = checkSimpleVar venv (varname, varpos)
                    in
                        case (#ty ty) of TY.RECORD (tylist, _) =>
                                         let
                                             val innerType = checkRecord(symbol, pos, tylist)
                                         in
                                             {exp=(), ty=innerType}
                                         end
                                       | _ => (err(pos, S.name varname ^ " is not a RECORD"); {exp=(), ty=TY.BOTTOM})
                    end
                  (* nested FieldVar, recurse *)
                  | A.FieldVar (var', symbol', pos') =>
                    let
                        val innerType = checkFieldVar venv tenv (var', symbol', pos')
                    in
                        case #ty innerType of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist)}
                                            | _ => (err(pos, "Inner type is not a RECORD"); {exp=(), ty=TY.BOTTOM})
                    end
                  (* nested SubscriptVar, recurse *)
                  | A.SubscriptVar(var', exp', pos') =>
                    let
                        val innerType = checkSubscriptVar venv tenv (var', exp', pos')
                    in
                        case #ty innerType of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist)}
                                            | _ => (err(pos, "Inner type is not a RECORD"); {exp=(), ty=TY.BOTTOM})
                    end
    end

(* check the type of subscriptVar, return expty *)
and checkSubscriptVar venv tenv (var, exp, pos) =
    case var of A.SimpleVar(varname, varpos) =>
                let
                    val ty = checkSimpleVar venv (varname, varpos)
                in
                    case #ty ty of TY.ARRAY (arrty, _) => {exp=(), ty=arrty}
                                 | _ => (err(pos, S.name varname ^ "is not an ARRAY"); {exp=(), ty=TY.BOTTOM})
                end
              | A.FieldVar (var', symbol', pos') =>
                let
                    val innerType = checkFieldVar venv tenv (var', symbol', pos')
                in
                    case #ty innerType of TY.ARRAY (ty, _) => {exp=(), ty=ty}
                                        | _ => (err(pos, "Expected ARRAY"); {exp=(), ty=TY.BOTTOM})
                end
              | A.SubscriptVar (var', exp', pos') =>
                let
                    val innerType = checkSubscriptVar venv tenv (var', exp', pos')
                in
                    case #ty innerType of TY.ARRAY (ty, _) => {exp=(), ty=ty}
                                        | _ => (err(pos, "Expected ARRAY"); {exp=(), ty=TY.BOTTOM})
                end

(* Checks whether ty1 and ty2 are compatible where
ty1 is the expected type, ty2 is the actual type, return bool *)
fun doCheckSameType (ty1, ty2, pos) =
    case ty1 of TY.RECORD(_, ty1Unique) =>
                (case ty2 of TY.RECORD(_, ty2Unique) =>
                             if ty1Unique = ty2Unique
                             then true
                             else (err(pos, "RECORD types mismatch"); false)
                           | TY.NIL => true
                           | TY.BOTTOM => true
                           | _ => (err(pos, "Expected RECORD type."); false))
              | TY.NIL =>
                (case ty2 of TY.BOTTOM => true
                           | _ => (err(pos, "Expected NIL type."); false))
              | TY.INT =>
                (case ty2 of TY.INT => true
                           | TY.BOTTOM => true
                           | _ => (err(pos, "Expected INT type."); false))
              | TY.STRING =>
                (case ty2 of TY.STRING => true
                           | TY.BOTTOM => true
                           | _ => (err(pos, "Expected STRING type."); false))
              | TY.BOTTOM => true
              | TY.ARRAY(_, ty1Unique) =>
                (case ty2 of TY.ARRAY(_, ty2Unique) =>
                             if ty1Unique = ty2Unique
                             then true
                             else (err(pos, "ARRAY types mismatch"); false)
                           | TY.BOTTOM => true
                           | _ => (err(pos, "Expected ARRAY type."); false))
              | TY.UNIT =>
                (case ty2 of TY.UNIT => true
                           | TY.BOTTOM => true
                           | _ => (err(pos, "Expected RECORD type."); false))
              | _ => false

(* check if ty is INT, return bool *)
fun checkInt ({exp,ty},pos) = doCheckSameType(TY.INT, ty, pos)

fun checkSameType ({exp=_, ty=ty1}, {exp=_, ty=ty2}, pos) = doCheckSameType(ty1, ty2, pos)

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
                (checkInt(trexp(leftExp), pos);
                 checkInt(trexp(rightExp), pos);
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
          | trexp (A.SeqExp ((exp:A.exp, pos:Absyn.pos)::left)) = (trexp(exp); trexp(A.SeqExp left))
          | trexp (A.AssignExp {var, exp, pos}) =
            let
                val varTy = trvar (var)
                val isLoopVaribale = checkIsLoopVariable(var, venv, pos)
                val expTy = trexp(exp)
                val _ = checkSameType(varTy, expTy, pos)
            in
              if isLoopVaribale
              then errAndBottom(pos, "can't assign to loop varibale")
              else {exp=(), ty=TY.UNIT}
            end
          | trexp (A.IfExp {test, then', else', pos}) =
            let
                val testIsInt = checkInt(trexp(test), pos)
                val _ = if testIsInt then () else err(pos, "Condition in an if-statement must be INT")
                val thenType = #ty (trexp then')
                (* If there is an else-clause, get its type. Otherwise, assume
                   return TY.BOTTOM, so it type checks with the then-clause. *)
                val elseType = if isSome(else') then #ty (trexp(valOf else')) else TY.BOTTOM
                val isSameType = doCheckSameType(thenType, elseType, pos)
                val _ = if isSameType then () else err(pos, "then- and else-clauses types mismatch")
            in
                {exp=(), ty=thenType}
            end
          | trexp (A.CallExp {func, args, pos}) =
            (case S.look(venv, func)
              of SOME(E.FunEntry {formals, result}) =>
                 let
                     val args': TY.ty list = map #ty (map trexp args)
                     val argsLength = length args'
                     val formalsLength = length formals
                     val _ = if argsLength <> formalsLength
                             then err(pos, "Function "
                                           ^ S.name func
                                           ^ " expects "
                                           ^ Int.toString(formalsLength)
                                           ^ " argument(s), but "
                                           ^ Int.toString(argsLength)
                                           ^ " were given.")
                             else ListPair.app (fn (ty1, ty2) =>
                                                   let val _ = doCheckSameType(ty1, ty2, pos)
                                                   in ()
                                                   end) (formals, args')
                 in
                     {exp=(), ty=result}
                 end
               | _ => (err(pos, S.name func ^ " not defined or is not a FUNCTION");
                       {exp=(), ty=TY.BOTTOM}))
          | trexp (A.RecordExp {fields, typ, pos}) =
            let
                val recordTy = S.look(tenv, typ)
                val fieldsLength = length fields
            in
                case recordTy
                 of SOME(TY.RECORD (symbols, unique)) =>
                    (if fieldsLength = 0
                     then {exp=(), ty=valOf recordTy}
                     else
                         let
                             val symbolsLength = length symbols
                             fun checkSymbols ((defSymbol, defType), (actualSymbol, exp, pos)) =
                                 let
                                     val _ = (if defSymbol <> actualSymbol
                                              then err(pos, S.name actualSymbol ^ " is not declared as a parameter")
                                              else ())
                                     val actualType = #ty (trexp(exp))
                                     val _ = doCheckSameType(defType, actualType, pos)
                                 in
                                     ()
                                 end
                             val _ = if fieldsLength <> symbolsLength
                                     then err(pos, "Record "
                                                   ^ S.name typ
                                                   ^ " has "
                                                   ^ Int.toString(symbolsLength)
                                                   ^ " fields, but "
                                                   ^ Int.toString(fieldsLength)
                                                   ^ " were given.")
                                     else ListPair.app checkSymbols (symbols, fields)
                         in
                             {exp=(), ty=valOf recordTy}
                         end)
                  | _ => (err(pos, S.name typ ^ " is not defined or is not a RECORD");
                          {exp=(), ty=TY.BOTTOM})
            end
          | trexp (A.WhileExp {test, body, pos}) =
            let
                val _ = checkInt(trexp(test), pos)
                val bodyTy = trexp(body)
                val isNoValue = doCheckSameType(#ty bodyTy, TY.UNIT, pos)
            in
                if isNoValue
                then {exp=(), ty=TY.UNIT}
                else errAndBottom(pos, "while body must produce no value")
            end

          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
            let
              val newVenv = S.enter(venv, var, E.VarEntry{ty=TY.INT, loopVar=true})
              val _ = doCheckSameType(TY.INT, #ty (trexp(lo)), pos)
              val _ = doCheckSameType(TY.INT, #ty (trexp(hi)), pos)
              val bodyTy = #ty (transExp(newVenv, tenv, body))
              val isNoValue = doCheckSameType(TY.UNIT, bodyTy, pos)
            in
              if isNoValue
              then {exp=(), ty=TY.UNIT}
              else errAndBottom(pos, "for body must produce no value")
            end
          | trexp (A.ArrayExp {typ, size, init, pos}) =
            if not (doCheckSameType(TY.INT, #ty (trexp size), pos))
            then (err(pos, "Size of an array must be INT"); {exp=(), ty=TY.BOTTOM})
            else
                let
                    val initType = trexp(init)
                    val arrayType = S.look(tenv, typ)
                in
                    case arrayType
                     of SOME(TY.ARRAY (ty, _)) =>
                        if doCheckSameType(ty, #ty initType, pos)
                        then {exp=(), ty=valOf arrayType}
                        else (err(pos, "init-exp and array type mismatch");
                              {exp=(), ty=TY.BOTTOM})
                      | _ => errAndBottom(pos, S.name typ ^ " is not an ARRAY")
                end
          | trexp _ = {exp=(), ty=TY.BOTTOM}

        (* get the type of provided var, return expty *)
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
    case dec of A.VarDec {name=name, typ=typ, init=init, pos=pos, ...} =>
                let
                    val {exp=_, ty=initType} = transExp(venv, tenv, init)
                in
                    case typ of NONE =>
                                (* If a variable type is not specified, simply take whatever type the
                                   expression returns and enter it into venv as a VarEntry *)
                                (case initType of TY.NIL =>
                                                  (err(pos, "Long form must be used if init-exp is NIL");
                                                   {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=TY.BOTTOM, loopVar=false})})
                                                | _ => {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=initType, loopVar=false})})
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
                                                         {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=symbolType, loopVar=false})}
                                                     end
                                                   | NONE =>
                                                     (err(pos, "Type " ^ S.name symbol ^ " is not found");
                                                      {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=TY.BOTTOM, loopVar=false})})
                                end

                end
              | A.FunctionDec (decs) =>
                let
                    fun transParam {name, typ, pos, escape} =
                        case S.look(tenv, typ) of SOME t => {name=name, ty=t}
                                                | NONE =>
                                                  (err(pos, "Type " ^ S.name typ ^ " not found"); {name=name, ty=TY.BOTTOM})
                    fun enterParam ({name, ty}, venv) = S.enter(venv, name, E.VarEntry{ty=ty, loopVar=false})
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
                        let
                          val bodyTy = #ty (transExp(venv, tenv, body))
                        in
                          if (doCheckSameType(TY.UNIT, bodyTy, pos))
                          then {venv=venv, tenv=tenv}
                          else (err(pos, "procedure's body must return unit"); {venv=venv, tenv=tenv})
                        end
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
