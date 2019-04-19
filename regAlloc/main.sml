structure Main = struct

structure Tr = Translate
structure F = MipsFrame
structure R = RegAlloc
structure A = Assem

fun emit out instrs =
    let (* todo: call procEntryExit3 *)
        val format0 = FormatAssem.format(MipsFrame.regToString(MipsFrame.tempMap))
        fun printInstr (instr, i) =
            (TextIO.output(out, Int.toString i ^ ": " ^ format0 instr); i + 1)
    in
        if (!Log.loglevel) <= Log.DEBUG then
            (TextIO.output(out, "============== Assembly =============\n");
             foldl printInstr 0 instrs;
             ())
        else ()
    end

fun dataflow out instrs =
    let val (flowgraph, nodes) = MakeGraph.instrs2graph instrs
        val igraph = Liveness.interferenceGraph flowgraph
        val interference = Liveness.getInterference(igraph)
        val Flow.FGRAPH {control=control} = flowgraph
        fun printFlowGraphNode (nodeID, info) =
            let val {def, use, ismove} = info
                val def = "[" ^ Utils.join ", " (map Int.toString def) ^ "]"
                val use = "[" ^ Utils.join ", " (map Int.toString use) ^ "]"
            in
                nodeID ^ ": Def: " ^ def ^ ", Use: " ^ use
            end
    in
        if (!Log.loglevel) <= Log.DEBUG then
            (TextIO.output(out, "============== Control Flow =============\n");
             Flow.Graph.printGraph printFlowGraphNode out control;
             interference)
             (* TextIO.output(out, "============== Liveness =============\n"); *)
             (* Liveness.show(out, igraph); interference) *)
        else interference
    end

fun rewriteProgram(instrs, frame, temp) =
    let val offset = F.allocInFrame frame
        val fp = F.getSpecialReg "fp"
        fun rewrite (instr, instrs) =
            case instr of A.OPER {assem, dst, src, jump} =>
                          (if Utils.inList (src, temp)
                           then (A.OPER {assem=MipsAssem.genI("lw", [temp], [fp], offset),
                                         dst=[temp],
                                         src=[],
                                         jump=NONE})::instr::instrs
                           else if Utils.inList (dst, temp)
                           then (instr::A.OPER {assem=MipsAssem.genI("sw", [], [temp, fp], offset),
                                                dst=[],
                                                src=[temp],
                                                jump=NONE}::instrs)
                           else (instr::instrs))
                        | _ => (instr::instrs)
    in
        foldr rewrite [] instrs
    end

fun allocRegs (out, instrs, frame) =
    let val interference = dataflow out instrs;
    in
        (interference, R.alloc (interference, frame))
    end
    handle (Color.spill temp) =>
           let val _ = Log.info("Temp " ^ Int.toString temp ^ " was spilled")
               val instrs = rewriteProgram(instrs, frame, temp)
               val _ = emit out instrs
           in
               allocRegs(out, instrs, frame)
           end


fun compileproc out (F.PROC{body,frame}, prevInstrs) =
    let val frameName = Symbol.name (F.name frame)
        val _ = print ("compiling " ^ frameName ^ "\n")
        val stms = (Canon.traceSchedule o Canon.basicBlocks o Canon.linearize) body
        val _ = if (!Log.loglevel) <= Log.DEBUG then
                    (TextIO.output(out, "============== Tree (" ^ frameName ^ ") ==============\n");
                     app (fn s => Printtree.printtree(out,s)) stms)
                else ()
        val entryExit = fn (instrs) => F.procEntryExit2(frame, instrs)
        val codegen = MipsGen.codegen frame
        val instrs = entryExit(List.concat(map codegen stms))
        val _ = emit out instrs
        val (interference, allocation) = allocRegs (out, instrs, frame)
        val temps = map KeyGraph.getNodeID (KeyGraph.nodes interference)
        val _ = if (!Log.loglevel) <= Log.DEBUG then
                    R.showAllocations(allocation, temps, out)
                else [()]
        val format0 = FormatAssem.format(MipsFrame.regToString(allocation))
        fun printInstr (instr, i) =
            (TextIO.output(out, Int.toString i ^ ": " ^ format0 instr); i + 1)
        val _ = (TextIO.output(out, "============== Real Assembly =============\n");
           foldl printInstr 0 instrs)
    in
        prevInstrs@instrs
    end
  | compileproc out (F.STRING(lab,s), instrs) = (TextIO.output(out, "\""^F.string(lab,s)^"\""); instrs)

fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
       handle e => (TextIO.closeOut out; raise e)
    end

fun compile filename =
    let val _ = Tr.frags := []
        val out = TextIO.openOut (filename ^ ".s")
        val absyn = Parse.parse filename
        val _ = if (!Log.loglevel) <= Log.DEBUG then
                    (TextIO.output(out, "================ AST ===============\n");
                     PrintAbsyn.print(out, absyn))
                else ()
        val frags = (Temp.reset(); FindEscape.findEscape absyn; Semant.transProg absyn)
        val instrs = foldl (compileproc out) [] frags
    in
        TextIO.closeOut out
    end

fun main(args) =
    if (length args) < 1 then
        (Log.error "Usage: tc FILENAME.tig";
         OS.Process.exit(OS.Process.failure))
    else
        let val _ = if (length args) = 2 then
                        let
                            val loglevel = valOf (Int.fromString (hd (tl args)))
                        in
                            Log.loglevel := loglevel
                        end
                    else ()
        in
            compile (hd args);
            OS.Process.exit(OS.Process.success)
        end

end

val _ = if not (String.isSuffix "sml" (CommandLine.name()))
        then Main.main(CommandLine.arguments())
        else ()
