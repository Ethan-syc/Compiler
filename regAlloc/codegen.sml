structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
structure F = MipsFrame
structure T = Tree
structure A = Assem
structure MA = MipsAssem

val ZERO = F.getSpecialReg "ZERO"
val RV = F.getSpecialReg "RV"
val SP = F.getSpecialReg "SP"

fun codegen frame stm =
    let val ilist = ref []
        fun emit x = ilist := x :: !ilist
        fun result (gen) = let val t = Temp.newtemp() in gen t; t end
        fun munchStm (T.SEQ(a, b)) = (munchStm(a); munchStm(b))
          | munchStm (T.LABEL(l)) = emit(A.LABEL{assem=MA.genLabel(l), lab=l})
          | munchStm (T.JUMP(T.NAME lab, labs)) = emit(A.OPER {assem=MA.genJ("j", lab), dst=[], src=[], jump=SOME(labs)})
          | munchStm (T.JUMP(_)) = Log.debug("Compiler error: jump instruction without label")
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
          | munchStm (T.CJUMP(T.EQ, e1, e2, l1, l2)) =
            let val src = [munchExp e1, munchExp e2]
            in
                emit(A.OPER {assem=MA.genB("beq", src, l1),
                             dst=[],
                             src=src,
                             jump=SOME([l1])})
            end
          | munchStm (T.CJUMP(T.NE, e1, e2, l1, l2)) =
            let val src = [munchExp e1, munchExp e2]
            in
                emit(A.OPER {assem=MA.genB("bne", src, l1),
                             dst=[],
                             src=src,
                             jump=SOME([l1])})
            end
          | munchStm (T.CJUMP(T.LT, e1, T.CONST i, l1, l2) |
                      T.CJUMP(T.GE, T.CONST i, e1, l1, l2)) =
            let val e1 = munchExp e1
                val t = Temp.newtemp()
            in
                emit(A.OPER {assem=MA.genI("slti", [t], [e1], i),
                             dst=[t],
                             src=[e1],
                             jump=NONE});
                emit(A.OPER {assem=MA.genB("bne", [t, ZERO], l1),
                             dst=[],
                             src=[t, ZERO],
                             jump=SOME([l1])})
            end
          | munchStm (T.CJUMP(T.LT, e1, e2, l1, l2) |
                      T.CJUMP(T.GT, e2, e1, l1, l2)) =
            let val e1 = munchExp e1
                val e2 = munchExp e2
                val t = Temp.newtemp()
            in
                emit(A.OPER {assem=MA.genR("slt", [t], [e1, e2], NONE),
                             dst=[t],
                             src=[e1, e2],
                             jump=NONE});
                emit(A.OPER {assem=MA.genB("bne", [t, ZERO], l1),
                             dst=[],
                             src=[t, ZERO],
                             jump=SOME([l1])})
            end
          | munchStm (T.CJUMP(T.LE, e1, e2, l1, l2) |
                      T.CJUMP(T.GE, e2, e1, l1, l2)) =
            let val src = [munchExp e1, munchExp e2]
                val t = Temp.newtemp()
            in
                emit(A.OPER {assem=MA.genR("sub", [t], src, NONE),
                             dst=[t],
                             src=src,
                             jump=NONE});
                emit(A.OPER {assem=MA.genB("blez", [t], l1),
                             dst=[],
                             src=[t],
                             jump=SOME([l1])})
            end
          | munchStm (T.CJUMP(relop, _, _, _, _)) =
            Log.debug("Compiler error: RELOP " ^ Utils.relop relop ^ " was never generated.")
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
          | munchStm (T.MOVE(T.TEMP t, T.CONST i)) =
            (* MIPS: addi $t, $0, i *)
            emit(A.OPER {assem=MA.genI("addi", [t], [ZERO], i),
                         dst=[t],
                         src=[ZERO],
                         jump=NONE})
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
            Log.debug("Compiler error: moving into constant")
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
        and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)) |
                      T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
            let val e1 = munchExp(e1)
            in
                result(fn (r) => emit(A.OPER {assem=MA.genI("lw", [r], [e1], i),
                                              dst=[r],
                                              src=[e1],
                                              jump=NONE}))
            end
          | munchExp (T.MEM(e)) =
            let val e = munchExp(e)
            in
                result(fn (r) => emit(A.OPER {assem=MA.genI("lw", [r], [e], 0),
                                              dst=[r],
                                              src=[e],
                                              jump=NONE}))
            end
          | munchExp (T.TEMP i) = i
          | munchExp (T.ESEQ(stm, exp)) = (munchStm(stm); munchExp(exp))
          | munchExp (T.NAME(label)) =
            result(fn (r) => emit(A.OPER {assem=MA.genLA("la", [r], label),
                                          dst=[r],
                                          src=[],
                                          jump=NONE}))
          | munchExp (T.CONST i) =
            result(fn (r) => emit(A.OPER {assem=MA.genI("addi", [r], [ZERO], i),
                                          dst=[r],
                                          src=[ZERO],
                                          jump=NONE}))
          | munchExp (T.CALL(T.NAME label, args)) =
            let fun munchArgs (i, nil) = nil
                  | munchArgs (i, (arg::args)) =
                    let val arg = munchExp(arg)
                        val dst = F.arg i
                        val _ = munchStm(T.MOVE(dst, T.TEMP arg))
                    in
                        case dst of T.TEMP t => t::munchArgs(i + 1, args)
                                  | _ => munchArgs(i + 1, args)
                    end
                val calldefs = F.getSpecialRegs "calldefs"
                val r = Temp.newtemp()
                val len = length args
            in
                if len > 0 then
                    emit (A.OPER {assem=MA.genI("addi", [SP], [SP], ~4 * (length args)),
                                  dst=[SP],
                                  src=[SP],
                                  jump=NONE})
                else ();
                emit (A.OPER {assem=MA.genJ("jal", label),
                              dst=calldefs,
                              src=munchArgs(0, args),
                              jump=SOME([label])});
                emit (A.OPER {assem=MA.genMove(r, RV),
                              dst=[r],
                              src=[RV],
                              jump=NONE});
                if len > 0 then
                    emit (A.OPER {assem=MA.genI("addi", [SP], [SP], 4 * (length args)),
                                  dst=[SP],
                                  src=[SP],
                                  jump=NONE})
                else ();
                r
            end
          | munchExp (T.CALL(_)) =
            (Log.debug("Compiler error: call destination not a label");
             Temp.newtemp())
          | munchExp (T.BINOP(T.PLUS, e, T.CONST i) |
                      T.BINOP(T.PLUS, T.CONST i, e)) =
            let val e = munchExp e
            in
                result(fn (r) => emit(A.OPER {assem=MA.genI("addi", [r], [e], i),
                                              dst=[r],
                                              src=[e],
                                              jump=NONE}))
            end
          | munchExp (T.BINOP(T.PLUS, e1, e2)) =
            let val e1 = munchExp(e1)
                val e2 = munchExp(e2)
            in
                result(fn (r) => emit(A.OPER {assem=MA.genR("add", [r], [e1, e2], NONE),
                                              dst=[r],
                                              src=[e1, e2],
                                              jump=NONE}))
            end
          | munchExp (T.BINOP(T.MINUS, e, T.CONST i)) =
            let val e = munchExp e
            in
                result(fn (r) => emit(A.OPER {assem=MA.genI("addi", [r], [e], ~i),
                                              dst=[r],
                                              src=[e],
                                              jump=NONE}))
            end
          | munchExp (T.BINOP(T.MINUS, e1, e2)) =
            let val e1 = munchExp(e1)
                val e2 = munchExp(e2)
            in
                result(fn (r) => emit(A.OPER {assem=MA.genR("sub", [r], [e1, e2], NONE),
                                              dst=[r],
                                              src=[e1, e2],
                                              jump=NONE}))
            end
          | munchExp (T.BINOP(T.MUL, e1, T.CONST i) |
                      T.BINOP(T.MUL, T.CONST i, e1)) =
            let val e1 = munchExp(e1)
                val power = IntInf.log2 (IntInf.fromInt i)
            in
                if IntInf.toInt (IntInf.pow(IntInf.fromInt 2, power)) = i
                then result(fn (r) =>
                               emit(A.OPER {assem=MA.genR("sll", [r], [e1], SOME(power)),
                                            dst=[r],
                                            src=[e1],
                                            jump=NONE}))
                else
                    let val r = Temp.newtemp()
                    in
                        emit(A.OPER {assem=MA.genI("addi", [r], [ZERO], i),
                                     dst=[r],
                                     src=[ZERO],
                                     jump=NONE});
                        munchExp(T.BINOP(T.MUL, T.TEMP e1, T.TEMP r))
                    end
            end
          | munchExp (T.BINOP(T.MUL, e1, e2)) =
            let val src = [munchExp e1, munchExp e2]
                val result = Temp.newtemp()
            in
                emit(A.OPER {assem=MA.genR("mult", [], src, NONE),
                             dst=[],
                             src=src,
                             jump=NONE});
                emit(A.OPER {assem=MA.genR("mflo", [result], [], NONE),
                             dst=[result],
                             src=[],
                             jump=NONE});
                result
            end
          | munchExp (T.BINOP(T.DIV, e, T.CONST i)) =
            let val e = munchExp(e)
                val power = IntInf.log2 (IntInf.fromInt i)
            in
                if IntInf.toInt (IntInf.pow(IntInf.fromInt 2, power)) = i
                then result(fn (r) => emit(A.OPER {assem=MA.genR("sra", [r], [e], SOME(power)),
                                                   dst=[r],
                                                   src=[e],
                                                   jump=NONE}))
                else let val r = Temp.newtemp()
                     in
                        emit(A.OPER {assem=MA.genI("addi", [r], [ZERO], i),
                                     dst=[r],
                                     src=[ZERO],
                                     jump=NONE});
                        munchExp(T.BINOP(T.DIV, T.TEMP e, T.TEMP r))
                     end
            end
          | munchExp (T.BINOP(T.DIV, e1, e2)) =
            let val src = [munchExp e1, munchExp e2]
                val result = Temp.newtemp()
            in
                emit(A.OPER {assem=MA.genR("div", [], src, NONE),
                             dst=[],
                             src=src,
                             jump=NONE});
                emit(A.OPER {assem=MA.genR("mflo", [result], [], NONE),
                             dst=[result],
                             src=[],
                             jump=NONE});
                result
            end
          | munchExp (T.BINOP(binop, _, _)) =
            (Log.debug("Compiler error: BINOP " ^ Utils.binop binop ^ " was never generated.");
             Temp.newtemp())
    in
        munchStm stm;
        rev(!ilist)
    end
end
