structure Main = struct

structure Tr = Translate
structure F = MipsFrame
(* structure R = RegAlloc *)


fun compileproc out (F.PROC{body,frame}, instrs) =
    let val frameName = Symbol.name (F.name frame)
        val _ = print ("compiling " ^ frameName ^ "\n")
        val stms = (Canon.traceSchedule o Canon.basicBlocks o Canon.linearize) body
        val _ = if (!Log.loglevel) <= Log.DEBUG then
                    (TextIO.output(out, "============== Tree (" ^ frameName ^ ") ==============\n");
                     app (fn s => Printtree.printtree(out,s)) stms)
                else ()
        val entryExit = fn (instrs) => F.procEntryExit2(frame, instrs)
        val codegen = MipsGen.codegen frame
    in
        instrs@(entryExit (List.concat(map codegen stms)))
    end
  | compileproc out (F.STRING(lab,s), instrs) = (TextIO.output(out,F.string(lab,s)); instrs)

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
             TextIO.output(out, "============== Liveness =============\n");
             Liveness.show(out, igraph); interference)
        else interference
    end

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
        val _ = emit out instrs
        val interference = dataflow out instrs;
        val allocation = RegAlloc.alloc (interference)
        val _ = if (!Log.loglevel) <= Log.DEBUG then
          RegAlloc.showAllocations(allocation, interference, out)
          else [()]
        val format0 = FormatAssem.format(MipsFrame.regToString(allocation))
        fun printInstr (instr, i) =
            (TextIO.output(out, Int.toString i ^ ": " ^ format0 instr); i + 1)
        val _ = (TextIO.output(out, "============== Real Assembly =============\n");
           foldl printInstr 0 instrs)

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
