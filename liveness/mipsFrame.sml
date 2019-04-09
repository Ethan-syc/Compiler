structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp

type frame = {label:Temp.label, formals: access list, offset: int ref, numLocals: int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of Temp.label * string

structure T = Tree
structure S = Symbol
structure Tab = Temp.Table

val _ = Temp.resetzero()
(* Stack pointer *)
val SP = Temp.newtemp()
(* Frame pointer *)
val FP = Temp.newtemp()
(* Return values *)
val V1 = Temp.newtemp()
val RV = Temp.newtemp()
(* Return address *)
val RA = Temp.newtemp()
(* Arguments *)
val A0 = Temp.newtemp()
val A1 = Temp.newtemp()
val A2 = Temp.newtemp()
val A3 = Temp.newtemp()
(* Temporaries *)
val T0 = Temp.newtemp()
val T1 = Temp.newtemp()
val T2 = Temp.newtemp()
val T3 = Temp.newtemp()
val T4 = Temp.newtemp()
val T5 = Temp.newtemp()
val T6 = Temp.newtemp()
val T7 = Temp.newtemp()
val T8 = Temp.newtemp()
val T9 = Temp.newtemp()
(* Saved registers *)
val S0 = Temp.newtemp()
val S1 = Temp.newtemp()
val S2 = Temp.newtemp()
val S3 = Temp.newtemp()
val S4 = Temp.newtemp()
val S5 = Temp.newtemp()
val S6 = Temp.newtemp()
val S7 = Temp.newtemp()
(* Zero register *)
val ZERO = Temp.newtemp()

val specialregs = [SP, FP, RV, V1, RA, ZERO]
val argregs = [A0, A1, A2, A3]
val calleesaves = [S0, S1, S2, S3, S4, S5, S6, S7]
val callersaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
val calldefs = RV::RA::V1::callersaves

exception NoSuchReg

fun addSpecialReg ((temp, name), table) =
    Temp.Table.enter(table, temp, name)

val tempMap = foldl addSpecialReg Temp.Table.empty [
        (SP, "sp"),
        (FP, "fp"),
        (V1, "v1"),
        (RV, "v0"),
        (RA, "ra"),
        (A0, "a0"),
        (A1, "a1"),
        (A2, "a2"),
        (A3, "a3"),
        (T0, "t0"),
        (T1, "t1"),
        (T2, "t2"),
        (T3, "t3"),
        (T4, "t4"),
        (T5, "t5"),
        (T6, "t6"),
        (T7, "t7"),
        (T8, "t8"),
        (T9, "t9"),
        (S0, "s0"),
        (S1, "s1"),
        (S2, "s2"),
        (S3, "s3"),
        (S4, "s4"),
        (S5, "s5"),
        (S6, "s6"),
        (S7, "s7"),
        (ZERO, "0")]

fun string (label, s) = (Symbol.name label) ^ "\n" ^ s ^ "\n"

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
        (* Offset starts at 4 because old FP *)
        {label=name, formals=formals', offset=ref ~4, numLocals=ref 1}
    end

fun name {label, formals, offset, numLocals} = label

fun formals {label, formals, offset, numLocals} = formals

fun numLocals {label, formals, offset, numLocals} = !numLocals

(* Allocates new local variable in the frame *)
fun allocLocal {label, formals, offset, numLocals} escape =
    if not escape
    then (
        Log.debug("Allocating InReg for frame " ^ S.name label);
        InReg(Temp.newtemp())
    )
    else
        let
            val _ = numLocals := !numLocals + 1
            (* grows down *)
            val _ = offset := !offset - 4
        in
            Log.debug("Allocating InFrame("
                        ^ Int.toString (!offset)
                        ^ ") for frame "
                        ^ S.name label);
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
fun procEntryExit1 (frame, body) = PROC {body=T.SEQ(T.LABEL(#label frame), T.EXP body), frame=frame}
fun procEntryExit2 (frame, body) = body@[Assem.OPER {assem=fn (x) => "<function exit>\n",
                                                     src=[ZERO, RA, SP]@calleesaves,
                                                     dst=[],
                                                     jump=SOME([])}]
(* fun procEntryExit3 ({label, formals, offset, numLocals}, body) = *)
(*     let val numLocals' = !numLocals *)
(*         fun viewShift (arg', formal, moves) = *)
(*             let val formal = exp formal (T.TEMP SP) *)
(*                 val arg' = arg arg' *)
(*             in *)
(*                 (T.MOVE(formal, arg'))::moves *)
(*             end *)
(*         val args = Utils.range(0, length formals) *)
(*         val moves = ListPair.foldr viewShift [] (args, formals) *)
(*         val result = Temp.newtemp() *)
(*         val insns = (T.LABEL label)::moves *)
(*                     @ [T.MOVE(T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(4)), T.TEMP(FP)), *)
(*                        T.MOVE(T.TEMP(FP), T.TEMP(SP)), *)
(*                        T.MOVE(T.TEMP(SP), T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(numLocals' * 4))), *)
(*                        T.MOVE(T.TEMP(RV), body), *)
(*                        T.MOVE(T.TEMP(SP), T.BINOP(T.PLUS, T.TEMP(SP), T.CONST(numLocals' * 4))), *)
(*                        T.MOVE(T.TEMP(FP), T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(4))), *)
(*                        T.JUMP(T.TEMP(RA), [])] *)
(*     in *)
(*         PROC {body=Utils.seq insns, frame={label=label, formals=formals, offset=offset, numLocals=numLocals}} *)
(*     end *)
fun procEntryExit3 ({label=name, formals=formals, offset=_, numLocals=numLocals}, body: Assem.instr list) =
    {prolog="PROCEDURE " ^ Symbol.name name ^ "\n",
     body=body,
     epilog="END " ^ Symbol.name name ^ "\n"}
fun allocString (label, literal) =
    STRING (label, literal)
fun printFrag (stream, PROC {body=body, frame=_}) =
    Printtree.printtree(stream, body)
  | printFrag (stream, STRING (_, s)) = print("STRING FRAG: " ^ s ^ "\n")
fun regToString reg =
    case Tab.look(tempMap, reg) of SOME(s) => s
                                 | _ => Temp.makestring reg
fun getSpecialReg name =
    if Utils.toLower(name) = "fp" then FP
    else if Utils.toLower(name) = "sp" then SP
    else if Utils.toLower(name) = "v1" then V1
    else if Utils.inList(["v0", "rv"], Utils.toLower(name)) then RV
    else if Char.toLower (hd(explode name)) = #"t" then
        List.nth(callersaves, ord(hd(tl(explode name))) - ord(#"0"))
    else if Char.toLower (hd(explode name)) = #"s" then
        List.nth(calleesaves, ord(hd(tl(explode name))) - ord(#"0"))
    else if Utils.toLower(name) = "zero" then ZERO
    else if Utils.toLower(name) = "ra" then RA
    else raise NoSuchReg
fun getSpecialRegs name =
    case Utils.toLower(name) of "callersaves" => callersaves
                              | "calldefs" => calldefs
                              | ("calleesaves" | "temporaries") => calleesaves
                              | "argregs" => argregs
                              | "specialregs" => specialregs
                              | _ => raise NoSuchReg
end
