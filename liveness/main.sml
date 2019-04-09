structure Main = struct

structure Tr = Translate
structure F = MipsFrame
(* structure R = RegAlloc *)

fun compileproc out (F.PROC{body,frame}, instrs) =
    let val frameName = Symbol.name (F.name frame)
        val _ = print ("compiling " ^ frameName ^ "\n")
        val stms = (Canon.traceSchedule o Canon.basicBlocks o Canon.linearize) body
        (* val _ = TextIO.output(out, "============== Tree (" ^ frameName ^ ") ==============\n") *)
        (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val entryExit = fn (instrs) => F.procEntryExit2(frame, instrs)
        val codegen = MipsGen.codegen frame
    in
        instrs@(entryExit (List.concat(map codegen stms)))
    end
  | compileproc out (F.STRING(lab,s), instrs) = (TextIO.output(out,F.string(lab,s)); instrs)

fun emit out instrs =
    let (* todo: call procEntryExit3 *)
        val format0 = FormatAssem.format(Temp.makestring)
        (* val _ = TextIO.output(out, "============== Assembly =============\n"); *)
        fun printInstr (instr, i) =
            (TextIO.output(out, Int.toString i ^ ": " ^ format0 instr); i + 1)
    in
        (* foldl printInstr 0 instrs *)
        app (fn (instr) => TextIO.output(out, format0 instr)) instrs
    end

fun dataflow out instrs =
    let val (flowgraph, nodes) = MakeGraph.instrs2graph instrs
        val igraph = Liveness.interferenceGraph flowgraph
        val Flow.FGRAPH {control=control} = flowgraph
        fun printFlowGraphNode (nodeID, info) =
            let val {def, use, ismove} = info
                val def = "[" ^ Utils.join ", " (map Int.toString def) ^ "]"
                val use = "[" ^ Utils.join ", " (map Int.toString use) ^ "]"
            in
                nodeID ^ ": Def: " ^ def ^ ", Use: " ^ use
            end
    in
        TextIO.output(out, "============== Control Flow =============\n");
        Flow.Graph.printGraph printFlowGraphNode out control;
        TextIO.output(out, "============== Liveness =============\n");
        Liveness.show(out, igraph);
        ()
    end

fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
       handle e => (TextIO.closeOut out; raise e)
    end

fun compile filename =
    let val _ = Tr.frags := []
        val absyn = Parse.parse filename
        (* (TextIO.output(out, "================ AST ===============\n"); *)
        (*  PrintAbsyn.print(out, absyn); *)
        val frags = (Temp.reset(); FindEscape.findEscape absyn; Semant.transProg absyn)
        fun doCompile out =
            let val instrs = foldl (compileproc out) [] frags
            in
                emit out instrs;
                dataflow out instrs
            end
    in
        withOpenFile (filename ^ ".s")
	             (fn out => doCompile out)
    end

end

(* val args = CommandLine.arguments()
val _ = if (length args) <> 1 then
            (Log.error "Usage: tc FILENAME.tig";
             OS.Process.exit(OS.Process.failure))
        else
            (Main.compile (hd args);
             OS.Process.exit(OS.Process.success)) *)
