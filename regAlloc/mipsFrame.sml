structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
structure A = Assem
structure T = Tree
structure S = Symbol
structure Tab = Temp.Table
exception Impossible

type frame = {label:Temp.label, formals: access list, offset: int ref, numLocals: int ref, moves: T.stm}
datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of Temp.label * string
type register = string

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
val specialRegs = [
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
val tempMap = foldl addSpecialReg Tab.empty specialRegs

structure StringMap = BinaryMapFn(struct type ord_key = string
                                   val compare = String.compare end)
val reverseTempMap = foldl (fn ((temp, name), map) => StringMap.insert(map, name, temp)) StringMap.empty specialRegs

fun string (label, s) = (Symbol.name label) ^ "\n" ^ s ^ "\n"

fun arg 0 = T.TEMP(A0)
  | arg 1 = T.TEMP(A1)
  | arg 2 = T.TEMP(A2)
  | arg 3 = T.TEMP(A3)
  | arg n = T.MEM(T.BINOP(T.PLUS, T.TEMP(SP), T.CONST(4 * n)))

fun newFrame {name: Temp.label, formals} =
    let
        fun buildFormals (i, []) = []
          (* Not escaping -> register *)
          | buildFormals (i, false::rest) =
            (InReg(Temp.newtemp()))::(buildFormals (i + 4, rest))
          | buildFormals (i, true::rest) =
            (InFrame i)::(buildFormals (i + 4, rest))
        fun genMoves (i, []) = []
          | genMoves (i, access::rest) =
            case access of InReg(temp) => T.MOVE(T.TEMP temp, arg i)::genMoves(i + 1, rest)
                         | InFrame(offset) => T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP FP, T.CONST offset)), arg i)::genMoves(i + 1, rest)
        val formals' = buildFormals (0, formals)
        val moves = genMoves(0, formals')
    in
        (* Offset starts at -8 because old FP is saved at -4($sp) *)
        {label=name, formals=formals', offset=ref ~4, numLocals=ref 1, moves=Utils.seq moves}
    end

fun name {label, formals, offset, numLocals, moves} = label

fun formals {label, formals, offset, numLocals, moves} = formals

fun numLocals {label, formals, offset, numLocals, moves} = !numLocals

(* Allocates new local variable in the frame *)
fun allocLocal {label, formals, offset, numLocals, moves} escape =
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

fun allocInFrame frame =
    case allocLocal frame true of InFrame(offset) => offset
                                | _ => raise Impossible
fun regToString table reg =
    case Tab.look(table, reg)
     of SOME(s) => s
      | _ =>
        (case Tab.look(tempMap, reg) of SOME(s) => s
                                      | _ => Temp.makestring reg)
fun exp access treeExp =
    case access of InReg(temp) => T.TEMP(temp)
                 | InFrame(offset) => T.MEM(T.BINOP(T.PLUS, treeExp, T.CONST(offset)))
fun externalCall (s, args) =
    T.CALL(T.NAME(Temp.namedlabel s), args)
fun procEntryExit1 (frame, body) =
    let val moves = #moves frame
        val body = Utils.seq [moves, T.MOVE(T.TEMP RV, body)]
    in
        PROC {body=body, frame=frame}
    end

fun saveCallee (frame, allTemps, allocation, instrs) =
    let fun find (temp, temps): Temp.temp list =
            case Tab.look(allocation, temp)
             of SOME(color) =>
                (case StringMap.find(reverseTempMap, color)
                  of SOME(colorTemp) =>
                     if Utils.inList(calleesaves, colorTemp)
                     then colorTemp::temps
                     else temps)
              | NONE => temps
        fun genInst (inst, temp, imm) format =
            let val temp = format(temp)
                val addr = format(FP)
            in
                inst ^ " $" ^ temp ^ ", " ^ Utils.i2s imm ^ "($" ^ addr ^ ")\n"
            end
        fun save (temp, offset, instrs) =
            (A.OPER {assem=genInst("sw", temp, offset),
                     dst=[],
                     src=[temp, FP],
                     jump=NONE})::instrs
        fun restore (temp, offset, instrs) =
            (A.OPER {assem=genInst("lw", temp, offset),
                     dst=[temp],
                     src=[FP],
                     jump=NONE})::instrs
        val toSave = foldl find [] allTemps
        val _ = Log.info("Frame "
                         ^ S.name (#label frame)
                         ^ " used "
                         ^ Utils.i2s (length toSave)
                         ^ " S-regs: "
                         ^ Utils.join ", " (map Utils.i2s toSave))
        val raOffset = allocInFrame frame
        val offset = map (fn _ => allocInFrame frame) toSave
        val saveInstrs = (A.OPER {assem=genInst("sw", RA, raOffset),
                                  dst=[],
                                  src=[RA],
                                  jump=NONE})::(ListPair.foldl save [] (toSave, offset))
        val restoreInstrs = (A.OPER {assem=genInst("lw", RA, raOffset),
                                     dst=[RA],
                                     src=[],
                                     jump=NONE})::(ListPair.foldl restore [] (toSave, offset))
    in
        saveInstrs@instrs@restoreInstrs
    end

fun procEntryExit2 (frame: frame, body) =
    body@[Assem.OPER {assem=fn (x) => "",
                      src=[ZERO, RA, SP],
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
fun procEntryExit3 ({label=name, offset=offset, ...}: frame, body: Assem.instr list) =
    {prolog="# PROCEDURE " ^ S.name name ^ "\n"
            ^ S.name name ^ ":\n"
            ^ "sw $fp, -4($sp)\n"
            ^ "addi $fp, $sp, 0\n"
            ^ "addi $sp, $sp, " ^ Utils.i2s (!offset) ^ "\n",
     body=body,
     epilog="addi $sp, $sp, " ^ Utils.i2s (~(!offset)) ^ "\n"
            ^ "lw $fp, -4($sp)\n"
            ^ "jr $ra\n"
            ^ "# END " ^ S.name name ^ "\n"}
fun allocString (label, literal) =
    STRING (label, literal)
fun printFrag (stream, PROC {body=body, frame=_}) =
    Printtree.printtree(stream, body)
  | printFrag (stream, STRING (_, s)) = print("STRING FRAG: " ^ s ^ "\n")
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
