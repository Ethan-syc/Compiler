structure Main = struct

structure Tr = Translate
structure F = MipsFrame
structure R = RegAlloc
structure A = Assem
structure S = Symbol

exception Compiler of string
val OUT_DIR = "out/"
val VER_STRING = "ECE 553 TigC 0.1.0 beta"

fun reset () =
    (Tr.frags := [];
     strings := [];
     ErrorMsg.reset();
     Temp.reset())

fun parse out filename =
    let val absyn = Parse.parse filename
    in
        (ErrorMsg.die();
         case out of SOME(out) =>
                     (TextIO.output(out, "==== AST ====\n");
                      PrintAbsyn(out, absyn))
                   | _ => ();
         absyn)
    end

fun semant absyn =
    let val _ = FindEscape.findEscape absyn
        val frags = Semant.transProg absyn
    in
        (ErrorMsg.die();
         frags)
    end

fun dataflow out debugOut instrs =
    let val (flowgraph, nodes) = MakeGraph.instrs2graph instrs
        val igraph = Liveness.interferenceGraph flowgraph
        val interference = Liveness.getInterference(igraph)
        val Flow.FGRAPH {control=control} = flowgraph
        fun printFlowGraphNode (nodeID, info) =
            let val {def, use, ismove} = info
                val def = ListFormat.listToString Int.toString def
                val use = ListFormat.listToString Int.toString use
            in
                nodeID ^ ": Def: " ^ def ^ ", Use: " ^ use
            end
    in
        case debugOut of SOME(debug) =>
                         (Utils.emit debug "==== Control Flow ====\n";
                          Flow.Graph.printGraph printFlowGraphNode out control;
                          Utils.emit debug "==== Liveness ====\n";
                          Liveness.show(out, igraph))
                       | _ => ();
        interference
    end

fun writeHeader out =
    (TextIO.output(out, "# file = " ^ filename ^ "\n");
     TextIO.output(out, "# " ^ VER_STRING ^ "\n");
     TextIO.output(out, "# Compiled at " ^ (Date.fmt "%c" (Date.fromTimeLocal(Time.now()))) ^ "\n"))

fun writeRuntime out =
    let val runtime = TextIO.inputAll (TextIO.openIn "runtimele.s")
        val sysspim = TextIO.inputAll (TextIO.openIn "sysspim.s")
    in
        TextIO.output(out, "################### RUNTIME ################\n");
        TextIO.output(out, runtime);
        TextIO.output(out, "################## SYSTEM #################\n");
        TextIO.output(out, sysspim)
    end

fun writeTree out frame stms =
    (app (Utils.emit out) ["==== Tree (", S.name (F.name frame), "====\n"];
     app (fn s => Printtree.printtree(debug, s)) stms)

fun writeInstrs out instrs =
    let val format = FormatAssem.format(MipsFrame.regToString(MipsFrame.tempMap))
        fun printInstr(instr, i) =
            (app (Utils.emit out) [Int.toString i, ": ", format instr]; i + 1)
    in
        Utils.emit out "==== Assembly ====\n";
        foldl printInstr 0 instrs;
        ()
    end

fun compileproc out debugOut (F.PROC {body, frame}) =
    let val stms = tl (canonicalize body)
        val instrs = List.concat(map (MipsGen.codegen frame) stms)
        val instrs = F.procEntryExit2(frame, instrs)

        val _ = case debugOut of SOME(debug) =>
                                 (writeTree debug frame stms;
                                  writeInstrs debug instrs)
                               | _ => ()


fun compile filename =
    let val _ = reset()
        val outFile = OUT_DIR ^ OS.Path.file filename ^ ".s"
        val _ = (OS.FileSys.mkDir OUT_DIR) handle SysErr => () (* Dir exists ignore *)
        val debugOut = if (!Log.loglevel <= Log.DEBUG)
                       then SOME(TextIO.openOut (outFile ^ ".debug"))
                       else NONE
        val absyn = parse debugOut filename
        val frags = semant(absyn)
        val out = TextIO.openOut(outFile)
        val _ = writeHeader(out)
        val instrs = concat(map (compileproc out) frags)

end
