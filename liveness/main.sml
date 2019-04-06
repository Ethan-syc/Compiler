structure Main = struct

structure Tr = Translate
structure F = MipsFrame
(* structure R = RegAlloc *)

fun emitproc out (F.PROC{body,frame}) =
    let val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")
        val stms = (Canon.traceSchedule o Canon.basicBlocks o Canon.linearize) body
        (* val _ = TextIO.output(out, "============== Tree ==============\n") *)
        (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val entryExit = fn (instrs) => F.procEntryExit2(frame, instrs)
        val codegen = MipsGen.codegen frame
	val instrs = List.concat(map (entryExit o codegen) stms)
        (* todo: call procEntryExit3 *)
        val format0 = FormatAssem.format(Temp.makestring)
    in
        (* TextIO.output(out, "============== Assembly =============\n"); *)
        app (fn i => TextIO.output(out,format0 i)) instrs
    end
  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
       handle e => (TextIO.closeOut out; raise e)
    end

fun compile filename =
    let val _ = Tr.frags := []
        val absyn = Parse.parse filename
        val frags = (Temp.reset(); FindEscape.findEscape absyn; Semant.transProg absyn)
    in
        withOpenFile (filename ^ ".s")
	             (fn out =>
                         (* (TextIO.output(out, "================ AST ===============\n"); *)
                         (*  PrintAbsyn.print(out, absyn); *)
                         (app (emitproc out) frags))
    end

end
