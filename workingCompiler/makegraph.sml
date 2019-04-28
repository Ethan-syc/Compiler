signature MAKEGRAPH =
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.info Flow.Graph.node list
end

structure MakeGraph:MAKEGRAPH =
struct
structure F = Flow
structure A = Assem
structure G = F.Graph
structure S = Symbol

fun getNodeID(instr, i) =
    case instr of A.OPER {assem, dst, src, jump} => Int.toString i
                | A.LABEL {assem, lab} => S.name lab
                | A.MOVE {assem, dst, src} => Int.toString i

fun getInfo(instr) =
    case instr of A.OPER {assem, dst, src, jump} =>
                  let
                      val def = dst
                      val use = src
                      val ismove = false
                  in
                      {def=def, use=use, ismove=ismove}
                  end
                | A.LABEL {assem, lab} =>
                  let
                      val def = []
                      val use = []
                      val ismove = false
                  in
                      {def=def, use=use, ismove=ismove}
                  end
                | A.MOVE {assem, dst, src} =>
                  let
                      val def = [dst]
                      val use = [src]
                      val ismove = true
                  in
                      {def=def, use=use, ismove=ismove}
                  end

fun createNodes (graph, i, []) = (graph, [])
  | createNodes (graph, i, instr::rest) =
    let
        val (curGraph, curNodeList) = createNodes (graph, i + 1, rest)
        val nodeID = getNodeID(instr, i)
        val nodeInfo = getInfo(instr)
        val (newGraph, newNode) = G.addNode'(curGraph, nodeID, nodeInfo)
    in
        (newGraph, newNode::curNodeList)
    end
fun createEdges (graph, i, []) = (graph, [])
  | createEdges (graph, i, [instr]) = (graph, [G.getNode (graph, (getNodeID(instr, i)))])
  | createEdges (graph, i, instr::rest) =
    let
        val (graph, nextNode::rest) = createEdges(graph, i + 1, rest)
        val curNodeID = getNodeID(instr, i)
        val nextNodeID = G.getNodeID(nextNode)
        val curNode = G.getNode(graph, curNodeID)
        (* fall through *)
        val graph = G.addEdge(graph, {from=curNodeID, to=nextNodeID})
        (* jump labels *)
        val graph = case instr of A.OPER {assem, dst, src, jump} =>
                              (case jump of SOME(labels) =>
                                            let
                                                fun addEdgeForOneLabel(label, graph) =
                                                    let
                                                        val jumpNodeID = S.name label
                                                    in
                                                        (* exception? *)
                                                        G.addEdge(graph, {from=curNodeID, to=jumpNodeID})
                                                    end
                                                    handle NoSuchNode => graph
                                            in
                                                foldl addEdgeForOneLabel graph labels
                                            end
                                          | NONE => graph)
                            | _ => graph
    in
        (graph, curNode::nextNode::rest)
    end

fun instrs2graph (instrList) =
    let
        val graph = G.empty
        val (graph, nodeList) = createNodes(graph, 0, instrList)
        val (graph, nodeList) = createEdges(graph, 0, instrList)
    in
        (F.FGRAPH {control=graph}, nodeList)
    end
end
