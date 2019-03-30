structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
structure F = MipsFrame
structure T = Tree
structure A = Assem
structure MA = MipsAssem

val ZERO = F.getSpecialReg "ZERO"

fun codegen frame stm =
    let val ilist = ref []
        fun emit x = ilist := x :: !ilist
        fun result (gen) = let val t = Temp.newtemp() in gen t; t end
        fun munchStm (T.SEQ(a, b)) = (munchStm(a); munchStm(b))
          | munchStm (T.LABEL(l)) = emit(A.LABEL{assem=MA.genLabel(l), lab=l})
          | munchStm (T.JUMP(T.NAME lab, labs)) = emit(A.OPER {assem=MA.genJ("j", lab), dst=[], src=[], jump=SOME(labs)})
          | munchStm (T.JUMP(_)) = Utils.debug("Compiler error: jump instruction without label")
          | munchStm (T.CJUMP(T.LE, e, T.CONST 0, l1, l2) |
                      T.CJUMP(T.GT, T.CONST 0, e, l1, l2)) =
            (* MIPS: blez e, l1 *)
            let val e = munchExp(e)
            in
                emit(A.OPER {assem=MA.genB("blez", [e], l1),
                             dst=[],
                             src=[e],
                             jump=SOME([l1])})
            end
          | munchStm (T.CJUMP(T.GT, e, T.CONST 0, l1, l2) |
                      T.CJUMP(T.LE, T.CONST 0, e, l1, l2)) =
            (* MIPS: bgtz e, l1 *)
            let val e = munchExp(e)
            in
                emit(A.OPER {assem=MA.genB("bgtz", [e], l1),
                             dst=[],
                             src=[e],
                             jump=SOME([l1])})
            end
          (* TODO: CJUMP with other relops *)
          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2) |
                      T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
            (* MIPS: sw e2, i(e1) *)
            let val src = [munchExp(e2), munchExp(e1)]
                val assem = MA.genI("sw", [], src, i)
            in
                emit(A.OPER {assem=assem, dst=[], src=src, jump=NONE})
            end
          | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
            (* MIPS: sw e2, i($0) *)
            let val e2 = munchExp(e2)
            in
                emit(A.OPER {assem=MA.genI("sw", [], [e2, ZERO], i),
                             dst=[],
                             src=[e2, ZERO],
                             jump=NONE})
            end
          | munchStm (T.MOVE(T.MEM(e1), e2)) =
            (* MIPS: sw e2, 0(e1) *)
            let val e2 = munchExp(e2)
                val e1 = munchExp(e1)
            in
                emit(A.OPER {assem=MA.genI("sw", [], [e2, e1], 0),
                             dst=[],
                             src=[e2, e1],
                             jump=NONE})
            end
          | munchStm (T.MOVE(T.TEMP i, e)) =
            (* MIPS: addi $i, e, 0 *)
            let val e = munchExp(e)
            in
                emit(A.OPER {assem=MA.genI("addi", [i], [e], 0),
                             dst=[i],
                             src=[e],
                             jump=NONE})
            end
          | munchStm (T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, T.CONST i, e2))) |
                      T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, e2, T.CONST i)))) =
            (* MIPS: lw e1, i(e2) *)
            let val e1 = munchExp(e1)
                val e2 = munchExp(e2)
            in
                emit(A.OPER {assem=MA.genI("lw", [e1], [e2], i),
                             dst=[e1],
                             src=[e2],
                             jump=NONE})
            end
          | munchStm (T.MOVE(T.CONST i, _)) =
            Utils.debug("Compiler error: moving into constant")
          | munchStm (T.MOVE(e1, e2)) =
            (* The most generic case *)
            (* MIPS: add e1, e2, $0 *)
            let val dst = munchExp(e1)
                val src = munchExp(e2)
            in
                emit(A.MOVE {assem=MA.genMove(dst, src),
                             dst=dst,
                             src=src})
            end
          | munchStm (T.EXP e) = (munchExp(e); ())
        and munchExp e = Temp.newtemp()
    in
        munchStm stm;
        rev(!ilist)
    end
end
