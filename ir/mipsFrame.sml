structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp

type frame = {label:Temp.label, formals: access list, offset: int ref, numLocals: int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of Temp.label * string

structure T = Tree

val SP = Temp.newtemp()
val FP = Temp.newtemp()
val RV = Temp.newtemp()
val RA = Temp.newtemp()
val A0 = Temp.newtemp()
val A1 = Temp.newtemp()
val A2 = Temp.newtemp()
val A3 = Temp.newtemp()

fun newFrame {name: Temp.label, formals} =
    let
        val offsetPos = ref 4   (* This is offset from FP, plus 4 for the static link *)
        fun buildFormals (formals:bool list):access list =
            case formals of [] => []
                          (* not escape -> register *)
                          | false::rest => (InReg(Temp.newtemp()))::(buildFormals rest)
                          (* escape -> in frame *)
                          | true::rest =>
                            let
                                val offset = !offsetPos
                                val _ = offsetPos := !offsetPos + 4
                            in
                                (InFrame offset)::(buildFormals rest)
                            end
        (* InFrame 0 is the static link *)
        val formals' = (InFrame 0)::(buildFormals formals)
    in
        {label=name, formals=formals', offset=ref 0, numLocals=ref 1}
    end

fun name {label, formals, offset, numLocals} = label

fun formals {label, formals, offset, numLocals} = formals

fun numLocals {label, formals, offset, numLocals} = !numLocals

(* Allocates new local variable in the frame *)
fun allocLocal {label, formals, offset, numLocals} escape =
    if not escape
    then (InReg(Temp.newtemp()))
    else
        let
            val _ = numLocals := !numLocals + 1
            (* grows down *)
            val _ = offset := !offset - 4
        in
            InFrame(!offset)
        end

fun exp access treeExp =
    case access of InReg(temp) => T.TEMP(temp)
                 | InFrame(offset) => T.MEM(T.BINOP(T.PLUS, treeExp, T.CONST(offset)))
fun arg 0 = T.TEMP(A0)
  | arg 1 = T.TEMP(A1)
  | arg 2 = T.TEMP(A2)
  | arg 3 = T.TEMP(A3)
  | arg n = T.MEM(T.BINOP(T.PLUS, T.TEMP(SP), T.CONST(4 * n)))
fun externalCall (s, args) =
    T.CALL(T.NAME(Temp.namedlabel s), args)
fun procEntryExit1 (frame, body) = PROC {body=T.EXP body, frame=frame}
fun procEntryExit3 ({label, formals, offset, numLocals}, body) =
    let val numLocals' = !numLocals
        fun viewShift (arg', formal, moves) =
            let val formal = exp formal (T.TEMP SP)
                val arg' = arg arg'
            in
                (T.MOVE(formal, arg'))::moves
            end
        val args = Utils.range(0, length formals)
        val moves = ListPair.foldr viewShift [] (args, formals)
        val result = Temp.newtemp()
        val insns = (T.LABEL label)::moves
                    @ [T.MOVE(T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(4)), T.TEMP(FP)),
                       T.MOVE(T.TEMP(FP), T.TEMP(SP)),
                       T.MOVE(T.TEMP(SP), T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(numLocals' * 4))),
                       T.MOVE(T.TEMP(RV), body),
                       T.MOVE(T.TEMP(SP), T.BINOP(T.PLUS, T.TEMP(SP), T.CONST(numLocals' * 4))),
                       T.MOVE(T.TEMP(FP), T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(4))),
                       T.JUMP(T.TEMP(RA), [])]
    in
        PROC {body=Utils.seq insns, frame={label=label, formals=formals, offset=offset, numLocals=numLocals}}
    end
fun allocString (label, literal) =
    STRING (label, literal)
end
