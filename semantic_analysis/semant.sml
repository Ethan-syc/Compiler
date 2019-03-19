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

fun symbolCompare (a, b) = String.compare(S.name a, S.name b)

structure Set = BinarySetFn(struct type ord_key = S.symbol
                                   val compare = symbolCompare
                            end)

fun err(pos,message) = ErrorMsg.error pos message

fun errAndBottom(pos, message) = (err(pos, message); {exp=(), ty=TY.BOTTOM})

(* TODO: Change this to false to suppress all messages *)
fun debug(message) = if false then print("DEBUG: " ^ message) else ()

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

fun cycleToString (l, on) =
    let
        fun join [] = ""
          | join (symbol::[]) = S.name symbol
          | join (symbol::rest) = S.name symbol ^ "->" ^ join(rest)
        val setStr = join(l)
    in
        setStr ^ "->" ^ S.name on
    end

fun actualTy symbol =
    let
        val _ = debug("Creating PENDING on " ^ S.name symbol ^ "\n")
    in
        fn (tenv, pos) =>
           let
               val _ = debug("Resolving PENDING on " ^ S.name symbol ^ "...\n")
               val ty = case S.look(tenv, symbol)
                         of SOME(ty) =>
                            (case ty of TY.PENDING(func) => func(tenv, pos)
                                      | _ => ty)
                          | NONE =>
                            (err(pos, "Type " ^ S.name symbol ^ " does not exist"); TY.BOTTOM)
               val _ = debug("\t" ^ TY.typeToString(ty) ^ "\n")
           in
               ty
           end
    end
fun checkIsLoopVariable (var, venv, pos) =
    case var of A.SimpleVar(varname, varpos) =>
                (case (S.look(venv, varname)) of SOME(E.VarEntry {ty, loopVar}) => loopVar
                                               | _ => false)
              | _ => false

(* check if the record has the symbol field, return the symbol type, return BOTTOM if not found *)
fun checkRecord (symbol, pos, l, tenv) =
    case l of [] => (err(pos, (S.name symbol) ^ " not found in RECORD"); TY.BOTTOM)
            | ((s, ty)::rest) =>
              let
                  val symbolname = S.name symbol
              in
                  if S.name s = symbolname then
                      case ty of TY.PENDING(func) => func(tenv, pos)
                               | ty => ty
                  else checkRecord(symbol, pos, rest, tenv)
              end

(* Checks whether ty1 and ty2 are compatible where
   ty1 is the expected type, ty2 is the actual type, return bool *)
fun doCheckSameType (tenv, ty1, ty2, pos) =
    let
        val ty1 = case ty1 of TY.PENDING(func) => func(tenv, pos)
                            | ty => ty
        val ty2 = case ty2 of TY.PENDING(func) => func(tenv, pos)
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
fun checkInt (tenv, {exp,ty},pos) = doCheckSameType(tenv, TY.INT, ty, pos)

fun checkSameType (tenv, {exp=_, ty=ty1}, {exp=_, ty=ty2}, pos) = doCheckSameType(tenv, ty1, ty2, pos)

(* check the type of the provided simpleVar, return expty *)
fun checkSimpleVar venv tenv (symbol, pos) =
    let
        val _ = debug("SimpleVar: " ^ S.name symbol ^ "\n")
    in
        case S.look(venv, symbol)
         of SOME(E.VarEntry entry) =>
            let

                val ty = #ty entry
            in
                (case ty of TY.PENDING(func) => {exp=(), ty=func(tenv, pos)}
                          | _ => {exp=(), ty=ty})
            end
          | _ => errAndBottom(pos, S.name symbol ^ " not declared or not a variable")
    end

(* check the type of the provided fieldVar, return expty *)
and checkFieldVar venv tenv (var, symbol, pos) =
    let
        val _ = debug("FieldVar: " ^ S.name symbol ^ "\n")
        fun err(pos, var, actualType) =
            errAndBottom(pos, "Expected "
                              ^ varToString var
                              ^ " to be a RECORD. Got "
                              ^ TY.typeToString(actualType)
                              ^ " instead")
    in
        (* the most simple case, var is simpleVar, type of simpleVar should be TY.RECORD if correct *)
        case var
         of A.SimpleVar(varname, varpos) =>
            let
                val ty = checkSimpleVar venv tenv (varname, varpos)
            in
                case (#ty ty) of TY.RECORD (tylist, _) =>
                                 let
                                     val innerType = case checkRecord(symbol, pos, tylist, tenv)
                                                      of TY.PENDING(func) => func(tenv, pos)
                                                       | ty => ty
                                 in
                                     {exp=(), ty=innerType}
                                 end
                               | _ => err(varpos, var, #ty ty)
            end
          (* nested FieldVar, recurse *)
          | A.FieldVar (var', symbol', pos') =>
            let
                val innerType = #ty (checkFieldVar venv tenv (var', symbol', pos'))
            in
                case innerType of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist, tenv)}
                                | TY.PENDING(func) =>
                                  let
                                      val actualType = func(tenv, pos)
                                  in
                                      case actualType
                                       of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist, tenv)}
                                        | _ => err(pos', var, actualType)
                                  end
                                | _ => err(pos', var, innerType)
            end
          (* nested SubscriptVar, recurse *)
          | A.SubscriptVar(var', exp', pos') =>
            let
                val innerType = checkSubscriptVar venv tenv (var', exp', pos')
            in
                case #ty innerType of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist, tenv)}
                                    | TY.PENDING(func) =>
                                      let
                                          val actualType = func(tenv, pos)
                                      in
                                          case actualType
                                           of TY.RECORD (tylist, _) => {exp=(), ty=checkRecord(symbol, pos, tylist, tenv)}
                                            | _ => err(pos', var, actualType)
                                      end
                                    | _ => err(pos', var, #ty innerType)
            end
    end

(* check the type of subscriptVar, return expty *)
and checkSubscriptVar venv tenv (var, exp, pos) =
    let
        fun err(pos, var, actualType) =
            errAndBottom(pos, "Expected "
                              ^ varToString var
                              ^ " to be an ARRAY. Got "
                              ^ TY.typeToString actualType
                              ^ " instead")
    in
        case var
         of A.SimpleVar(varname, varpos) =>
            let
                val ty = case #ty (checkSimpleVar venv tenv (varname, varpos))
                          of TY.PENDING(func) => func(tenv, pos)
                           | ty => ty
            in
                case ty of TY.ARRAY (arrty, _) => {exp=(), ty=arrty}
                         | _ => err(varpos, var, ty)
            end
          | A.FieldVar (var', symbol', pos') =>
            let
                val innerType = checkFieldVar venv tenv (var', symbol', pos')
            in
                case #ty innerType of TY.ARRAY (ty, _) => {exp=(), ty=ty}
                                    | TY.PENDING(func) =>
                                      let
                                          val actualType = func(tenv, pos)
                                      in
                                          case actualType of TY.ARRAY(ty, _) => {exp=(), ty=ty}
                                                           | _ => err(pos', var, actualType)
                                      end
                                    | _ => err(pos', var, #ty innerType)
            end
          | A.SubscriptVar (var', exp', pos') =>
            let
                val innerType = checkSubscriptVar venv tenv (var', exp', pos')
            in
                case #ty innerType of TY.ARRAY (ty, _) => {exp=(), ty=ty}
                                    | TY.PENDING(func) =>
                                      let
                                          val actualType = func(tenv, pos)
                                      in
                                          case actualType of TY.ARRAY(ty, _) => {exp=(), ty=ty}
                                                           | _ => err(pos', var, actualType)
                                      end
                                    | _ => err(pos', var, #ty innerType)
            end
    end

(* use a stack of bool to keep tracking the break status *)
val loopBroken: (bool ref list) ref = ref []

(* push a true into stack when entering a loop which indicate the loop is not broken *)
fun enterLoop() = loopBroken := ref true::(!loopBroken)

(* pop a bool out of stack when leaving a loop *)
fun exitLoop() =
    case !loopBroken of [] => debug("Compiler error: no loop to exit\n")
                      | hd::rest => loopBroken := rest

(* change the top of the stack to false if the top is true,
err if the loop is already broken or not in loop*)
fun breakLoop(pos) =
    case !loopBroken of [] => err(pos, "Break expression can only be used in a loop")
                      | hd::rest =>
                        if !hd then hd := false else err(pos, "Current loop is already broken")

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
                let
                    val left = checkInt(tenv, trexp(leftExp), pos);
                    val right = checkInt(tenv, trexp(rightExp), pos);
                    val _ = if (not left) orelse (not right)
                            then err(pos, "Arithmetic expressions may only be performed on INTs")
                            else ()
                in
                    {exp=(), ty=TY.INT}
                end
            else (* oper = EqOp orelse oper = NeqOp *)
                (checkSameType(tenv, trexp(leftExp), trexp(rightExp), pos);
                 {exp=(), ty=TY.INT})
          | trexp (A.VarExp var) = trvar(var)
          | trexp (A.LetExp {decs, body, pos}) =
            let
                val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
            in
                transExp(venv', tenv', body)
            end
          | trexp (A.SeqExp []) = {exp=(), ty=TY.UNIT}
          | trexp (A.SeqExp [(exp, pos)]) = trexp(exp)
          | trexp (A.SeqExp ((exp, pos)::rest)) = (trexp(exp); trexp(A.SeqExp rest))
          | trexp (A.AssignExp {var, exp, pos}) =
            let
                val varTy = trvar (var)
                val isLoopVaribale = checkIsLoopVariable(var, venv, pos)
                val expTy = trexp(exp)
                val _ = checkSameType(tenv, varTy, expTy, pos)
            in
                if isLoopVaribale
                then errAndBottom(pos, "Cannot assign to loop varibale " ^ varToString var)
                else {exp=(), ty=TY.UNIT}
            end
          | trexp (A.IfExp {test, then', else', pos}) =
            let
                val testIsInt = checkInt(tenv, trexp(test), pos)
                val _ = if testIsInt then () else err(pos, "Condition of an if-statement must be INT")
                val thenType = #ty (trexp then')
                (* If there is an else-clause, get its type. Otherwise, assume
                   return TY.BOTTOM, so it type checks with the then-clause. *)
                val elseType = if isSome(else') then #ty (trexp(valOf else')) else TY.BOTTOM
                val _ = if not (isSome(else')) andalso not (doCheckSameType(tenv, TY.UNIT, thenType, pos))
                        then err(pos, "If-then expression must produce no value")
                        else ()
                val isSameType = doCheckSameType(tenv, thenType, elseType, pos)
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
                                           ^ " arguments, but "
                                           ^ Int.toString(argsLength)
                                           ^ " were given.")
                             else ListPair.app (fn (ty1, ty2) =>
                                                   let
                                                       val isSameType = doCheckSameType(tenv, ty1, ty2, pos)
                                                   in
                                                       if not isSameType
                                                       then err(pos, "Argument types mismatch for function call to "
                                                                     ^ S.name func)
                                                       else ()
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
                fun checkRecordFields (symbols, unique) =
                    if fieldsLength = 0
                     then {exp=(), ty=valOf recordTy}
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
                                     val actualType = #ty (trexp(exp))
                                     val defType = case defType of TY.PENDING(func) => func(tenv, pos)
                                                                 | ty => ty
                                     val _ = doCheckSameType(tenv, defType, actualType, pos)
                                 in
                                     ()
                                 end
                             val _ = if fieldsLength <> symbolsLength
                                     then err(pos, "RECORD "
                                                   ^ S.name typ
                                                   ^ " has "
                                                   ^ Int.toString(symbolsLength)
                                                   ^ " fields, but "
                                                   ^ Int.toString(fieldsLength)
                                                   ^ " were given.")
                                     else ListPair.app checkSymbols (symbols, fields)
                         in
                             {exp=(), ty=valOf recordTy}
                         end
            in
                case recordTy
                 of SOME(TY.RECORD(symbols, unique)) => checkRecordFields(symbols, unique)
                  | SOME(TY.PENDING(func)) =>
                         (case func(tenv, pos) of TY.RECORD(symbols, unique) => checkRecordFields(symbols, unique)
                                                | _ => (err(pos, S.name typ ^ " is not defined or is not a RECORD");
                                                        {exp=(), ty=TY.BOTTOM}))
                  | _ => (err(pos, S.name typ ^ " is not defined or is not a RECORD");
                          {exp=(), ty=TY.BOTTOM})
            end
          | trexp (A.WhileExp {test, body, pos}) =
            let
                val _ = checkInt(tenv, trexp(test), pos)
                val _ = enterLoop()
                val bodyTy = trexp(body)
                val _ = exitLoop()
                val isNoValue = doCheckSameType(tenv, #ty bodyTy, TY.UNIT, pos)
            in
                if isNoValue
                then {exp=(), ty=TY.UNIT}
                else errAndBottom(pos, "While-loop body must produce no value.")
            end

          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
            let
                val newVenv = S.enter(venv, var, E.VarEntry{ty=TY.INT, loopVar=true})
                val _ = doCheckSameType(tenv, TY.INT, #ty (trexp(lo)), pos)
                val _ = doCheckSameType(tenv, TY.INT, #ty (trexp(hi)), pos)
                val _ = enterLoop()
                val bodyTy = #ty (transExp(newVenv, tenv, body))
                val _ = exitLoop()
                val isNoValue = doCheckSameType(tenv, TY.UNIT, bodyTy, pos)
            in
                if isNoValue
                then {exp=(), ty=TY.UNIT}
                else errAndBottom(pos, "For-loop body must produce no value.")
            end
          | trexp (A.ArrayExp {typ, size, init, pos}) =
            if not (doCheckSameType(tenv, TY.INT, #ty (trexp size), pos))
            then (err(pos, "Size of an array must be INT"); {exp=(), ty=TY.BOTTOM})
            else
                let
                    val initType = (case #ty (trexp(init)) of TY.PENDING(func) => func(tenv, pos)
                                                      | ty => ty)
                    val arrayType = S.look(tenv, typ)
                in
                    case arrayType of SOME(ty) =>
                                      let
                                          val actualType = case ty of TY.PENDING(func) => func(tenv, pos)
                                                                    | ty => ty

                                      in
                                         case actualType of TY.ARRAY(ty, _) =>
                                                            if doCheckSameType(tenv, ty, initType, pos)
                                                            then {exp=(), ty=actualType}
                                                            else errAndBottom(pos, "init-exp and array type mismatch")
                                                          | _ => errAndBottom(pos, S.name typ ^ " is not an ARRAY")
                                      end
                                    | NONE => errAndBottom(pos, "Type " ^ S.name typ ^ " not found")
                end
          | trexp (A.BreakExp pos) = (breakLoop(pos); {exp=(), ty=TY.UNIT})

        (* get the type of provided var, return expty *)
        and trvar (A.SimpleVar(symbol, pos)) = checkSimpleVar venv tenv (symbol, pos)
          | trvar (A.FieldVar(var, symbol, pos)) = checkFieldVar venv tenv (var, symbol, pos)
          | trvar (A.SubscriptVar(var, exp, pos)) =
            let
                val _ = checkInt(tenv, trexp(exp), pos)
            in
                checkSubscriptVar venv tenv (var, exp, pos)
            end
        and transDecs (venv: venvType, tenv: tenvType, decs: A.dec list) =
            foldl (fn (dec, {venv=venv, tenv=tenv}) => transDec(venv, tenv, dec)) {venv=venv, tenv=tenv} decs
        and transDec (venv, tenv, dec) =
            case dec of A.VarDec {name=name, typ=typ, init=init, pos=pos, ...} =>
                        let
                            val {exp=_, ty=initType} = transExp(venv, tenv, init)
                        in
                            case typ of NONE =>
                                        (* If a variable type is not specified, simply take whatever type the
                                           expression returns and enter it into venv as a VarEntry *)
                                        let
                                            val initType = case initType of TY.PENDING(func) => func(tenv, pos)
                                                                          | ty => ty
                                        in
                                            case initType of TY.NIL =>
                                                             (err(pos, "Long form must be used if init-exp is NIL.");
                                                              {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=TY.BOTTOM, loopVar=false})})
                                                           | TY.UNIT =>
                                                             (err(pos, "Cannot define a variable with no value.");
                                                              err(pos, "Hint: init-exp returns no value");
                                                              {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=TY.BOTTOM, loopVar=false})})
                                                           | _ => {tenv=tenv, venv=S.enter(venv, name, E.VarEntry{ty=initType, loopVar=false})}
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
                                                    val _ = debug(S.name symbol ^ " is " ^ TY.typeToString symbolType ^ "\n")
                                                    val _ = doCheckSameType(tenv, symbolType, initType, pos)
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
                                                          (err(pos, "Type " ^ S.name typ ^ " is not found"); {name=name, ty=TY.BOTTOM})
                            fun enterParam ({name, ty}, venv) = S.enter(venv, name, E.VarEntry{ty=ty, loopVar=false})
                            fun checkFunctionHeader ({name, params, body, pos, result}, venv) =
                                let
                                    val params' = map #ty (map transParam params)
                                    (* If a return type is specified, it must exist. Otherwise, it's a procedure
                                       returning TY.UNIT *)
                                    val result_ty = case result
                                                     of SOME(rt, _) =>
                                                        (case S.look(tenv, rt) of SOME(ty) => ty
                                                                                | NONE => (err(pos, "Type " ^ S.name rt ^ " is not found"); TY.BOTTOM))
                                                      | NONE => TY.UNIT
                                in
                                    S.enter(venv, name, E.FunEntry{formals=params', result=result_ty})
                                end
                            fun checkFunctionDec ({name, params, body: A.exp, pos, result}, {venv: venvType, tenv: tenvType}) =
                                let
                                    (* We are guaranteed to have this symbol in venv because checkFunctionHeader
                                       will have been run *)
                                    val entry = valOf (S.look(venv, name))
                                    val resultType = case entry of E.FunEntry{formals, result} => result
                                                                | _ => (debug("Compiler error: "
                                                                              ^ S.name name
                                                                              ^ " is not a FunEntry in venv");
                                                                        TY.BOTTOM)
                                    val params' = map transParam params
                                    val venv' = foldl enterParam venv params'
                                    val actualReturn = transExp(venv', tenv, body)
                                    val _ = if not (doCheckSameType(tenv, resultType, #ty actualReturn, pos))
                                    then
                                        (err(pos, "Function/procedure "
                                                 ^ S.name name
                                                 ^ " was declared to return "
                                                 ^ TY.typeToString(resultType)
                                                 ^ ", but "
                                                 ^ TY.typeToString(#ty actualReturn)
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
                            fun checkFunctionNames (funcDec: A.fundec, existingNames) =
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
                            val _ = foldl checkFunctionNames Set.empty decs
                            val venv' = foldl checkFunctionHeader venv decs
                        in
                            foldl checkFunctionDec {venv=venv', tenv=tenv} decs
                        end
                      | A.TypeDec decs =>
                        let
                            fun checkTypeDec ({name, ty, pos}, {venv=venv, tenv=tenv}): {venv: venvType, tenv: tenvType} =
                                case ty
                                 of A.NameTy(symbol, _) =>
                                    (case S.look(tenv, symbol)
                                      of SOME(ty) => {venv=venv, tenv=S.enter(tenv, name, ty)}
                                       | NONE =>
                                         let
                                             val ty = TY.PENDING(actualTy symbol)
                                         in
                                             {venv=venv, tenv=S.enter(tenv, name, ty)}
                                         end)
                                  | A.ArrayTy(symbol, _) =>
                                    (case S.look(tenv, symbol)
                                      of SOME(ty) => {venv=venv, tenv=S.enter(tenv, name, TY.ARRAY(ty, ref ()))}
                                       | NONE => {venv=venv, tenv=S.enter(tenv, name, TY.ARRAY(TY.PENDING(actualTy symbol), ref ()))})
                                  | A.RecordTy fields =>
                                    let
                                        fun checkField {name, escape, typ, pos} =
                                            case S.look(tenv, typ)
                                             of SOME(ty) => (name, ty)
                                              | NONE => (name, TY.PENDING(actualTy typ))
                                        fun checkFieldName ({name, escape, typ, pos}, existingNames) =
                                            if Set.member(existingNames, name)
                                            then (err(pos, "Redefinition of field with name "
                                                           ^ S.name name);
                                                  existingNames)
                                            else Set.add(existingNames, name)
                                        val _ = foldl checkFieldName Set.empty fields
                                    in
                                        {venv=venv, tenv=S.enter(tenv, name, TY.RECORD(map checkField fields, ref ()))}
                                    end
                            fun checkTypeName ({name, ty, pos}, existingNames) =
                                if Set.member(existingNames, name)
                                then (err(pos, "Type with name "
                                               ^ S.name name
                                               ^ " already exists in this mutually recursive type group");
                                      existingNames)
                                else Set.add(existingNames, name)
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
                            fun resolvePending tenv pos ty =
                                case ty of TY.PENDING(func) => (func(tenv, pos); ())
                                         | _ => ()
                            fun checkPending tenv {name, ty, pos} =
                                let
                                    val _ = debug("Looking for " ^ S.name name ^ " in tenv\n")
                                    val entry = S.look(tenv, name)
                                in
                                    case entry of SOME(ty) =>
                                                  (case ty of TY.PENDING(func) => (func(tenv, pos); ())
                                                            | TY.ARRAY(arrayType, _) => resolvePending tenv pos arrayType
                                                            | TY.RECORD(fields, _) => (map (resolvePending tenv pos) (map #2 fields); ())
                                                            | _ => ())
                                               | NONE => debug(S.name name ^ " not found in tenv\n")
                                end
                            val _ = foldl checkTypeName Set.empty decs
                            val result = foldl checkTypeDec {venv=venv, tenv=tenv} decs
                            val typeMap = foldl makeTypeMap S.empty decs
                            val cycleDetected = foldl (fn (tf, result) => result orelse tf) false (map (checkTypeCycle typeMap) decs)
                            (* check every dec onece after we add all the pending ty into the new tenv to make sure they can resolve correctly *)
                            val _ = if not cycleDetected then map (checkPending (#tenv result)) decs else []
                        in
                            result
                        end
    in
        trexp(exp)
    end

fun transProg (AST_expression:A.exp) =
    let
        val _ = transExp(E.base_venv, E.base_tenv, AST_expression)
    in
        ()
    end

end
