structure Semant =
struct

structure A = Absyn
structure TY = Types
structure S = Symbol
structure E = Env
structure TR = Translate
structure T = Tree

type venvType = E.enventry S.table
type tenvType = E.ty S.table
type expty = {exp:TR.exp, ty:TY.ty}
val nestedLevel = ref 0
fun symbolCompare (a, b) = String.compare(S.name a, S.name b)

structure Set = BinarySetFn(struct type ord_key = S.symbol
                                   val compare = symbolCompare
                            end)

fun err(pos,message) = ErrorMsg.error pos message

fun errAndBottom(pos, exp, message) = (err(pos, message); {exp=exp, ty=TY.BOTTOM})

(* TODO: Change this to false to suppress all messages *)
fun debug(message) = if true then print("DEBUG: " ^ message ^ "\n") else ()

fun varToString (A.SimpleVar(symbol, _)) = S.name symbol
  | varToString (A.FieldVar(var, symbol, _)) = varToString(var) ^ "." ^ (S.name symbol)
  | varToString (A.SubscriptVar(var, exp, _)) = varToString(var) ^ "[#]"

fun listExists compareFn (l, item) =
    let
        fun helper (x) = case compareFn(x, item) of EQUAL => true
                                                  | _ => false
    in
        List.exists helper l
    end

val symbolExists = listExists symbolCompare
val errorFunEntry = {level=TR.outermost, label=Temp.newlabel(), formals=[TY.BOTTOM], result=TY.BOTTOM}

fun cycleToString (l, on) =
    let
        fun join [] = ""
          | join (symbol::[]) = S.name symbol
          | join (symbol::rest) = S.name symbol ^ "->" ^ join(rest)
        val setStr = join(l)
    in
        setStr ^ "->" ^ S.name on
    end

fun resolvePending (TY.PENDING(func)) = func()
  | resolvePending ty = ty

fun checkIsLoopVariable (var, venv, pos) =
    case var of A.SimpleVar(varname, varpos) =>
                (case (S.look(venv, varname)) of SOME(E.VarEntry {access, ty, loopVar}) => loopVar
                                               | _ => false)
              | _ => false

(* check if the record has the symbol field, return the symbol offset and type, or BOTTOM if not found *)
fun checkRecord (offset, symbol, pos, l) =
    case l of [] => (err(pos, (S.name symbol) ^ " not found in RECORD"); (offset, TY.BOTTOM))
            | ((s, ty)::rest) =>
              let
                  val symbolname = S.name symbol
              in
                  if S.name s = symbolname then
                      case ty of TY.PENDING(func) => (offset, func())
                               | ty => (offset, ty)
                  else checkRecord(offset + 1, symbol, pos, rest)
              end

(* Checks whether ty1 and ty2 are compatible where
   ty1 is the expected type, ty2 is the actual type, return bool *)
fun doCheckSameType (ty1, ty2, pos) =
    let
        val ty1 = case ty1 of TY.PENDING(func) => func()
                            | ty => ty
        val ty2 = case ty2 of TY.PENDING(func) => func()
                            | ty => ty
    in
        case ty1 of TY.RECORD(_, ty1Unique) =>
                    (case ty2 of TY.RECORD(_, ty2Unique) =>
                                 if ty1Unique = ty2Unique
                                 then true
                                 else (err(pos, "RECORD types mismatch. Redefinition of record creates new record."); false)
                               | TY.NIL => true
                               | TY.BOTTOM => true
                               | _ => (err(pos, "Expected RECORD type or NIL. Got " ^ (TY.typeToString ty2)); false))
                  | TY.NIL =>
                    (case ty2 of TY.BOTTOM => true
                               | _ => (err(pos, "Expected NIL type. Got " ^ (TY.typeToString ty2)); false))
                  | TY.INT =>
                    (case ty2 of TY.INT => true
                               | TY.BOTTOM => true
                               | _ => (err(pos, "Expected INT type. Got " ^ (TY.typeToString ty2)); false))
                  | TY.STRING =>
                    (case ty2 of TY.STRING => true
                               | TY.BOTTOM => true
                               | _ => (err(pos, "Expected STRING type. Got " ^ (TY.typeToString ty2)); false))
                  | TY.BOTTOM => true
                  | TY.ARRAY(_, ty1Unique) =>
                    (case ty2 of TY.ARRAY(_, ty2Unique) =>
                                 if ty1Unique = ty2Unique
                                 then true
                                 else (err(pos, "ARRAY types mismatch. Redefinition of array type creates new array"); false)
                               | TY.BOTTOM => true
                               | _ => (err(pos, "Expected ARRAY type. Got " ^ (TY.typeToString ty2)); false))
                  | TY.UNIT =>
                    (case ty2 of TY.UNIT => true
                               | TY.BOTTOM => true
                               | _ => (err(pos, "Expected UNIT type. Got " ^ (TY.typeToString ty2)); false))
                  | TY.PENDING(_) => (debug("Compiler error: PENDING not resolved\n"); false)
    end

(* check if ty is INT, return bool *)
fun checkInt (tenv, {exp,ty},pos) = doCheckSameType(TY.INT, ty, pos)

fun checkSameType (tenv, {exp=_, ty=ty1}, {exp=_, ty=ty2}, pos) = doCheckSameType(ty1, ty2, pos)

val loopLevel = ref 0

fun enterLoop() = loopLevel := !loopLevel + 1

fun exitLoop() =
    case (!loopLevel) of 0 => debug("Compiler error: no loop to exit\n")
                       | _ => (loopLevel := !loopLevel - 1; ())

fun inLoop(pos) =
    case !loopLevel of 0 => err(pos, "Break expression can only be used in a loop")
                     | _ => ()

fun transExp (venv: venvType, tenv:tenvType, exp:A.exp, level: TR.level, break: Temp.label): expty =
    let
        (* Bind venv, tenv and level to these functions for use in trvar *)
        (* Note that these are the environments/levels when an assign stmt
           is executed, not necessarily when the variable was defined. *)
        fun trexp(A.NilExp) = {exp=TR.transConst 0, ty=TY.NIL}
          | trexp(A.IntExp i) = {exp=TR.transInt i, ty=TY.INT}
          | trexp(A.StringExp (s, pos)) =
            let
                val exp = TR.transString s
            in
                {exp=exp, ty=TY.STRING}
            end
          | trexp(A.OpExp {left=leftExp, oper=oper, right=rightExp, pos=pos}) =
            let
                val {exp=leftExp, ty=leftTy} = trexp(leftExp)
                val {exp=rightExp, ty=rightTy} = trexp(rightExp)
            in
                if (oper = A.PlusOp orelse
                    oper = A.MinusOp orelse
                    oper = A.TimesOp orelse
                    oper = A.DivideOp orelse
                    oper = A.LtOp orelse
                    oper = A.LeOp orelse
                    oper = A.GtOp orelse
                    oper = A.GeOp)
                then
                    let

                        val left = doCheckSameType(TY.INT, leftTy, pos)
                        val right = doCheckSameType(TY.INT, rightTy, pos)
                        val _ = if (not left) orelse (not right)
                                then err(pos, "Arithmetic expressions may only be performed on INTs")
                                else ()
                        val exp = TR.transBinOp(leftExp, rightExp, oper)
                    in
                        {exp=exp, ty=TY.INT}
                    end
                else (* oper = EqOp orelse oper = NeqOp *)
                    let
                        val _ = doCheckSameType(leftTy, rightTy, pos)
                    in
                        {exp=TR.transBinOp(leftExp, rightExp, oper), ty=TY.INT}
                    end
            end
          | trexp (A.VarExp var) = trvar(var)
          | trexp (A.LetExp {decs, body, pos}) =
            let
                val initExps = ref []
                val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs, level, break, initExps)
                val _ = initExps := rev(!initExps)
                val {exp=bodyExp, ty=bodyTy} = transExp(venv', tenv', body, level, break)
            in
                {exp=TR.transLet(!initExps, bodyExp), ty=bodyTy}
            end
          | trexp (A.SeqExp exps) =
            let
                val (exps, _) = ListPair.unzip exps
                val exptys = map trexp exps
                val exp = TR.transSeqExp(map #exp exptys)
            in
                if (length exps) = 0
                then {exp=exp, ty=TY.UNIT}
                else {exp=exp, ty=(#ty (List.last exptys))}
            end
          | trexp (A.AssignExp {var, exp, pos}) =
            let
                val {exp=varExp, ty=varTy} = trvar (var)
                val _ = case varExp of TR.Ex(T.ESEQ(_, T.MEM(_))) => Log.debug("is mem")
                                     | _ => Log.debug("is not mem")
                val isLoopVaribale = checkIsLoopVariable(var, venv, pos)
                val {exp=expExp, ty=expTy} = trexp(exp)
                val _ = doCheckSameType(varTy, expTy, pos)
                val exp = TR.transAssign(varExp, expExp)
            in
                if isLoopVaribale
                then errAndBottom(pos, exp, "Cannot assign to loop varibale " ^ varToString var)
                else {exp=exp, ty=TY.UNIT}
            end
          | trexp (A.IfExp {test, then', else', pos}) =
            let
                val {exp=testExp, ty=testTy} = trexp(test)
                val testIsInt = doCheckSameType(TY.INT, testTy, pos)
                val _ = if testIsInt then () else err(pos, "Condition of an if-statement must be INT")
                val {exp=thenExp, ty=thenType} = trexp(then')
                (* If there is an else-clause, get its type. Otherwise, assume
                   return TY.BOTTOM, so it type checks with the then-clause. *)
                val {exp=elseExp, ty=elseType} = if isSome(else')
                                                 then trexp(valOf else')
                                                 else {exp=TR.transConst 0, ty=TY.BOTTOM}
                val _ = if not (isSome(else')) andalso not (doCheckSameType(TY.UNIT, thenType, pos))
                        then err(pos, "If-then expression must produce no value")
                        else ()
                val isSameType = doCheckSameType(thenType, elseType, pos)
                val _ = if isSameType then () else err(pos, "then- and else-clauses types mismatch")
            in
                {exp=TR.transIf(testExp, thenExp, elseExp), ty=thenType}
            end
          | trexp (A.CallExp {func, args, pos}) =
            (case S.look(venv, func)
              of SOME(E.FunEntry {level=decLevel, label=label, formals=formals, result=result}) =>
                 let
                     val exptys = map trexp args
                     val argExps = map #exp exptys
                     val argTys = map #ty exptys
                     val argsLength = length argExps
                     val formalsLength = length formals
                     val _ = if argsLength <> formalsLength
                             then err(pos, "Function "
                                           ^ S.name func
                                           ^ " expects "
                                           ^ Int.toString(formalsLength)
                                           ^ " arguments, but "
                                           ^ Int.toString(argsLength)
                                           ^ " were given.")
                             else ListPair.app (fn (ty1, ty2) =>
                                                   let
                                                       val isSameType = doCheckSameType(ty1, ty2, pos)
                                                   in
                                                       if not isSameType
                                                       then err(pos, "Argument types mismatch for function call to "
                                                                     ^ S.name func)
                                                       else ()
                                                   end) (formals, argTys)
                 in
                     {exp=TR.transCall(level, label, decLevel, argExps), ty=result}
                 end
               | _ => (err(pos, S.name func ^ " not defined or is not a FUNCTION");
                       {exp=TR.transConst 0, ty=TY.BOTTOM}))
          | trexp (A.RecordExp {fields, typ, pos}) =
            let
                val recordTy = S.look(tenv, typ)
                val recordTy = case recordTy of SOME(ty) => resolvePending ty
                                              | _ => (err(pos, S.name typ ^ " is not defined."); TY.BOTTOM)
                val (symbols, unique) = case recordTy of TY.RECORD(symbols, unique) => (symbols, unique)
                                                       | _ => (err(pos, S.name typ ^ " is not a RECORD.");
                                                               ([], ref ()))
                val fieldsLength = length fields
            in
                if fieldsLength = 0
                then {exp=TR.transRecord(symbols, []), ty=recordTy}
                else
                    let
                        val symbolsLength = length symbols
                        fun checkSymbols ((defSymbol, defType), (actualSymbol, exp, pos)) =
                            let
                                val _ = (if defSymbol <> actualSymbol
                                         then err(pos, S.name actualSymbol
                                                       ^ " is not declared in RECORD "
                                                       ^ S.name typ)
                                         else ())
                                val {exp=actualExp, ty=actualType} = trexp(exp)
                                val defType = resolvePending defType
                                val _ = doCheckSameType(defType, actualType, pos)
                            in
                                {exp=actualExp, ty=actualType}
                            end
                        val exptys = if fieldsLength <> symbolsLength
                                     then (err(pos, "RECORD "
                                                    ^ S.name typ
                                                    ^ " has "
                                                    ^ Int.toString(symbolsLength)
                                                    ^ " fields, but "
                                                    ^ Int.toString(fieldsLength)
                                                    ^ " were given.");
                                           [])
                                     else ListPair.map checkSymbols (symbols, fields)
                        val actualFields = map #exp exptys
                    in
                        {exp=TR.transRecord(symbols, actualFields), ty=recordTy}
                    end
            end
          | trexp (A.WhileExp {test, body, pos}) =
            let
                val {exp=testExp, ty=testTy} = trexp(test)
                val _ = doCheckSameType(TY.INT, testTy, pos)
                val _ = enterLoop()
                val breakLabel = Temp.newlabel()
                val {exp=bodyExp, ty=bodyTy} = transExp(venv, tenv, body, level, breakLabel)
                val _ = exitLoop()
                val isNoValue = doCheckSameType(bodyTy, TY.UNIT, pos)
                val exp = TR.transWhile(bodyExp, testExp, breakLabel)
            in
                if isNoValue
                then {exp=exp, ty=TY.UNIT}
                else errAndBottom(pos, exp, "While-loop body must produce no value.")
            end

          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
            let
                val access = Translate.allocLocal level (!escape)
                val newVenv = S.enter(venv, var, E.VarEntry{access=access, ty=TY.INT, loopVar=true})
                val {exp=loExp, ty=loTy} = trexp(lo)
                val {exp=hiExp, ty=hiTy} = trexp(hi)
                val _ = doCheckSameType(TY.INT, loTy, pos)
                val _ = doCheckSameType(TY.INT, hiTy, pos)
                val _ = enterLoop()
                val breakLabel = Temp.newlabel()
                val {exp=bodyExp, ty=bodyTy} = transExp(newVenv, tenv, body, level, breakLabel)
                val _ = exitLoop()
                val isNoValue = doCheckSameType(TY.UNIT, bodyTy, pos)
                val loopVar = TR.simpleVar(access, level)
                val exp = TR.transFor(loopVar, loExp, hiExp, bodyExp, breakLabel)
            in
                if isNoValue
                then {exp=exp, ty=TY.UNIT}
                else errAndBottom(pos, exp, "For-loop body must produce no value.")
            end
          | trexp (A.ArrayExp {typ, size, init, pos}) =
            let
                val {exp=sizeExp, ty=sizeTy} = trexp size
                val _ = doCheckSameType(TY.INT, sizeTy, pos)
                val {exp=initExp, ty=initTy} = trexp init
                val arrayType = S.look(tenv, typ)
                val exp = TR.transArray(initExp, sizeExp)
            in
                case arrayType of SOME(TY.ARRAY(ty, unique)) =>
                                  if doCheckSameType(ty, initTy, pos)
                                  then {exp=exp, ty=TY.ARRAY(ty, unique)}
                                  else errAndBottom(pos, exp, "init-exp and array type mismatch")
                                | _ => errAndBottom(pos, exp, S.name typ ^ " is not defined or is not an ARRAY")
            end
          | trexp (A.BreakExp pos) = (inLoop(pos); {exp=TR.transBreak(break), ty=TY.UNIT})

        (* get the type of provided var, return expty *)
        and trvar (A.SimpleVar(symbol, pos)) =
            (* Level is when variable is used.  *)
            let
                val _ = Log.debug("SimpleVar: " ^ S.name symbol)
            in
                case S.look(venv, symbol)
                 of SOME(E.VarEntry {access, ty, loopVar}) =>
                    let
                        val exp = TR.simpleVar(access, level)
                    in
                        (case ty of TY.PENDING(func) => {exp=exp, ty=func()}
                                  | _ => {exp=exp, ty=ty})
                    end
                  | _ => errAndBottom(pos, TR.transConst 0, S.name symbol ^ " not declared or not a variable")
            end
          | trvar (A.FieldVar(var, symbol, pos)) =
            let
                val _ = Log.debug("FieldVar: " ^ S.name symbol)
                fun err(pos, var, exp, actualType) =
                    errAndBottom(pos, exp, "Expected "
                                           ^ varToString var
                                           ^ " to be a RECORD. Got "
                                           ^ TY.typeToString(actualType)
                                           ^ " instead")
                val {exp=exp, ty=ty} = trvar(var)
            in
                case ty of TY.RECORD (tylist, _) =>
                           let val (offset, ty) = checkRecord(0, symbol, pos, tylist)
                           in
                               {exp=TR.transFieldVar(exp, TR.transConst offset), ty=ty}
                           end
                         | TY.PENDING(func) =>
                           let
                                val actualType = func()
                           in
                                case actualType
                                 of TY.RECORD (tylist, _) =>
                                 (let val (offset, ty) = checkRecord(0, symbol, pos, tylist)
                                 in
                                     {exp=TR.transFieldVar(exp, TR.transConst offset), ty=ty}
                                 end)
                                  | _ => err(pos, var, exp, ty)
                            end
                         | _ => err(pos, var, exp, ty)
            end
          | trvar (A.SubscriptVar(var, exp, pos)) =
            let
                fun err(pos, var, exp, actualType) =
                    errAndBottom(pos, exp, "Expected "
                                           ^ varToString var
                                           ^ " to be an ARRAY. Got "
                                           ^ TY.typeToString actualType
                                           ^ " instead")
                val {exp=subExp, ty=subTy} = trexp(exp)
                val _ = doCheckSameType(subTy, TY.INT, pos)
                val {exp=exp, ty=ty} = trvar(var)
            in
                case ty of TY.ARRAY (ty, _) =>
                           let
                               val exp = TR.transSubscriptVar(exp, subExp)
                           in
                               {exp=exp, ty=ty}
                           end
                         (* | TY.PENDING(func) => *)
                         (*   let *)
                         (*       val actualType = func() *)
                         (*   in *)
                         (*       case actualType of TY.ARRAY(ty, _) => {exp=(), ty=ty} *)
                         (*                        | _ => err(pos', var, actualType) *)
                         (*   end *)
                         | _ => err(pos, var, exp, ty)

            end
        and transDecs (venv: venvType, tenv: tenvType, decs: A.dec list, level: Translate.level, break, initExps) =
            foldl (fn (dec, {venv=venv, tenv=tenv}) => transDec(venv, tenv, dec, level, break, initExps)) {venv=venv, tenv=tenv} decs
        and transDec (venv, tenv, dec, level, break, initExps) =
            case dec
             of A.VarDec {name, escape, typ, init, pos} =>
                let
                    val {exp=initExp, ty=initType} = transExp(venv, tenv, init, level, break)
                    val access = TR.allocLocal level (!escape)
                    val varExp = TR.simpleVar(access, level)
                    val exp = TR.transAssign(varExp, initExp)
                    val _ = initExps := exp::(!initExps)
                in
                    case typ of NONE =>
                                (* If a variable type is not specified, simply take whatever type the
                                           expression returns and enter it into venv as a VarEntry *)
                                let
                                    val initType = case initType of TY.PENDING(func) => func()
                                                                  | ty => ty
                                in
                                    case initType
                                     of TY.NIL =>
                                        (err(pos, "Variable type must be explicitly named if init-exp is NIL.");
                                         {tenv=tenv,
                                          venv=S.enter(venv, name, E.VarEntry{
                                                           access=access,
                                                           ty=TY.BOTTOM,
                                                           loopVar=false})})
                                      | TY.UNIT =>
                                        (err(pos, "Cannot define a variable with no value.");
                                         err(pos, "Hint: init-exp returns no value");
                                         {tenv=tenv,
                                          venv=S.enter(venv, name, E.VarEntry{
                                                           access=access,
                                                           ty=TY.BOTTOM,
                                                           loopVar=false})})
                                      | _ => {tenv=tenv,
                                              venv=S.enter(venv, name, E.VarEntry{
                                                               access=access,
                                                               ty=initType,
                                                               loopVar=false})}
                                end
                              | SOME(symbol, pos) =>
                                (* If a variable type is specified, first check the type of the init exp.
                                   If init exp returns NIL, then check if symbol represents a RECORD.
                                   Otherwise, check if symbol names the type returned by init exp *)
                                let
                                    val symbolTy = S.look(tenv, symbol)
                                in
                                    case symbolTy
                                     of SOME(symbolType) =>
                                        let
                                            val _ = Log.debug(S.name symbol ^ " is " ^ TY.typeToString symbolType)
                                            val _ = doCheckSameType(symbolType, initType, pos)
                                        in
                                            {tenv=tenv,
                                             venv=S.enter(venv, name, E.VarEntry{
                                                              access=access,
                                                              ty=symbolType,
                                                              loopVar=false})}
                                        end
                                      | NONE =>
                                        (err(pos, "Type " ^ S.name symbol ^ " is not found");
                                         {tenv=tenv,
                                          venv=S.enter(venv, name, E.VarEntry{access=access,
                                                                              ty=TY.BOTTOM,
                                                                              loopVar=false})})
                                end

                end
              | A.FunctionDec (decs) =>
                let fun checkFunctionNames (funcDec: A.fundec, existingNames) =
                        let
                            val name = #name funcDec
                            val pos = #pos funcDec
                        in
                            if Set.member(existingNames, name)
                            then (err(pos, "Function with name "
                                           ^ S.name name
                                           ^ " already exists in this mutually recursive function group");
                                  existingNames)
                            else Set.add(existingNames, name)
                        end
                    fun checkFunctionHeader ({name, params, body, pos, result}, venv) =
                        let
                            fun checkParamType {name, escape, typ, pos} =
                                case S.look(tenv, typ) of SOME(t) => t
                                                        | NONE => (err(pos, "Type "
                                                                            ^ S.name typ
                                                                            ^ " is not found");
                                                                   TY.BOTTOM)
                            val label = Temp.newlabel()
                            val formals = map op! (map #escape params)
                            val newLevel = TR.newLevel {parent=level, name=label, formals=formals}
                            val _ = Log.info("Function "
                                             ^ S.name name
                                             ^ "(" ^ S.name name ^ ")"
                                             ^ " creates level "
                                             ^ Int.toString (TR.depth newLevel))
                            val paramTypes = map checkParamType params
                            (* If a return type is specified, it must exist. Otherwise, it's a procedure
                               returning TY.UNIT *)
                            val result_ty = case result
                                             of SOME(rt, _) =>
                                                (case S.look(tenv, rt)
                                                  of SOME(ty) => ty
                                                   | NONE => (err(pos, "Type "
                                                                       ^ S.name rt
                                                                       ^ " is not found");
                                                              TY.BOTTOM))
                                              | NONE => TY.UNIT
                        in
                            (* We need to enter the function into the original venv, *)
                            (* not the one with its params, i.e. venv' *)
                            S.enter(venv,
                                    name,
                                    E.FunEntry{
                                        level=newLevel,
                                        label=label,
                                        formals=paramTypes,
                                        result=result_ty})
                        end
                    val _ = foldl checkFunctionNames Set.empty decs
                    val venv' = foldl checkFunctionHeader venv decs
                    fun checkFunctionDec ({name, params, body: A.exp, pos, result}, {venv: venvType, tenv: tenvType}) =
                        let
                            (* We are guaranteed to have this symbol in venv because checkFunctionHeader
                               will have been run *)
                            val {level=newLevel, label=_, formals=paramTypes, result=resultType} =
                                case valOf (S.look(venv, name))
                                 of E.FunEntry entry => entry
                                  | _ => (Log.error("Compiler error: "
                                                    ^ S.name name
                                                    ^ " is not a FunEntry in venv");
                                          errorFunEntry)
                            val accesses = TR.formals newLevel
                            fun enterParam ({name=name, ...}: A.field, (typ, access), venv) =
                                S.enter(venv, name, E.VarEntry {access=access, ty=typ, loopVar=false})
                            val venv' = ListPair.foldl enterParam venv (params, ListPair.zipEq (paramTypes, accesses))
                            val {exp=body, ty=actualReturn} = transExp(venv', tenv, body, newLevel, break)
                            val _ = TR.procEntryExit(newLevel, body)
                            val _ = if not (doCheckSameType(resultType, actualReturn, pos))
                                    then
                                        (err(pos, "Function/procedure "
                                                  ^ S.name name
                                                  ^ " was declared to return "
                                                  ^ TY.typeToString(resultType)
                                                  ^ ", but "
                                                  ^ TY.typeToString(actualReturn)
                                                  ^ " was returned instead.");
                                         case resultType of TY.UNIT =>
                                                            err(pos, "Hint: functions declared without "
                                                                     ^ "a return type must return "
                                                                     ^ "no value.")
                                                          | _ => ())
                                    else ()
                        in
                            {venv=venv, tenv=tenv}
                        end

                in
                    foldl checkFunctionDec {venv=venv', tenv=tenv} decs
                end
              | A.TypeDec decs => transTy(venv, tenv, decs)
    in
        trexp(exp)
    end
and transTy(venv, tenv, decs) =
    let
        fun makeTypeMap ({name, ty, pos}, types) =
            case S.look(types, name)
             of SOME(_) => (err(pos, "Type " ^
                                     S.name name
                                     ^ " is already defined in this mutually "
                                     ^"recursive function group");
                            types)
              | NONE => S.enter(types, name, {name=name, ty=ty, pos=pos})
        val typeDecMap = foldl makeTypeMap S.empty decs

        fun makeUniques ({name, ty, pos}, uniques) =
            case ty of A.RecordTy _ => S.enter(uniques, name, ref ())
                     | A.ArrayTy _ => S.enter(uniques, name, ref ())
                     | _ => uniques
        val uniques = foldl makeUniques S.empty decs

        fun actualTy (symbol, pos) =
            case S.look(typeDecMap, symbol) of SOME(dec) => processTypeDec(dec)
                                             | NONE =>
                                               (case S.look(tenv, symbol)
                                                 of SOME(ty) => ty
                                                  | NONE => (err(pos, "Type " ^ S.name symbol ^ " does not exist");
                                                             TY.BOTTOM))
        and processTypeDec ({name, ty, pos}) =
            case ty
             of A.NameTy(symbol, _) => actualTy(symbol, pos)
              | A.ArrayTy(symbol, _) =>
                let
                    val unique = case S.look(uniques, name)
                                  of SOME(unique) => unique
                                   | NONE => (Log.debug("Compiler error: cannot find unique for ARRAY "
                                                   ^ S.name name);
                                              ref ());
                in
                    TY.ARRAY(actualTy(symbol, pos), unique)
                end
              | A.RecordTy fields =>
                let
                    fun genRecord () =
                        let fun helper {name, escape, typ, pos} = (name, actualTy(typ, pos))
                            val fieldList = map helper fields
                            val unique = case S.look(uniques, name)
                                          of SOME(unique) => unique
                                           | NONE => (Log.debug("Compiler error: cannot find unique for "
                                                            ^ S.name name);
                                                      ref ())
                        in
                            TY.RECORD(fieldList, unique)
                        end
                    fun checkFieldName ({name, escape, typ, pos}, existingNames) =
                        if Set.member(existingNames, name)
                        then (err(pos, "Redefinition of field with name "
                                       ^ S.name name);
                              existingNames)
                        else Set.add(existingNames, name)
                    val _ = foldl checkFieldName Set.empty fields
                in
                   TY.PENDING(genRecord)
                end
        fun makeTypeMap ({name, ty, pos}, m) =
            case ty
             of A.NameTy(symbol, _) => S.enter(m, name, symbol)
              | A.ArrayTy(symbol, _) => S.enter(m, name, symbol)
              | _ => m
        exception Cycle
        fun checkTypeCycle m {name, ty, pos} =
            let
                fun helper (name, l) =
                    case S.look(m, name)
                     of SOME(symbol) =>
                        if symbolExists(l, symbol)
                        then (err(pos, "Type cycle detected: " ^ cycleToString(l, symbol));
                              err(pos, "Hint: type cycles cannot pass through "
                                       ^ "named types and array types");
                              raise Cycle)
                        else helper(symbol, l@[symbol])
                      | NONE => l
            in
                (helper(name, []); false)
            end
            handle Cycle => true
        fun checkTypeDec (dec, {venv, tenv}) =
            {venv=venv, tenv=S.enter(tenv, #name dec, processTypeDec(dec))}
        val result = foldl checkTypeDec {venv=venv, tenv=tenv} decs
        val typeMap = foldl makeTypeMap S.empty decs
        val cycleDetected = foldl (fn (tf, result) => result orelse tf) false (map (checkTypeCycle typeMap) decs)
        (* check every dec onece after we add all the pending ty into the new tenv to make sure they can resolve correctly *)

    in
        result
    end


fun transProg (AST_expression:A.exp) =
    let
        val level = TR.newLevel {parent=TR.outermost, name=Temp.namedlabel("tig_main"), formals=[]}
        val breakLabel = Temp.newlabel()
        val {exp=exp, ty=ty} = transExp(E.base_venv, E.base_tenv, AST_expression, level, breakLabel)
        val _ = Log.debug("Final type: " ^ TY.typeToString(ty))
        val mainFrag = TR.procEntryExit(level, exp)
    in
        TR.getResult()
    end

end
