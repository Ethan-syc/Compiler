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
  fun createEmptyGraph() = G.empty

  val curID = ref 0
  fun getNodeID(instr) =
    case instr of A.OPER {assem, dst, src, jump} => (curID := !curID + 1; Int.toString (!curID))
                | A.LABEL {assem, lab} => S.name lab
                | A.MOVE {assem, dst, src} => (curID := !curID + 1; Int.toString (!curID))
  fun resetNodeId() = curID := 0

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

  fun createNodes (graph, []) = (graph, [])
    | createNodes (graph, instr::rest) =
      let
        val (curGraph, curNodeList) = createNodes (graph, rest)
        val nodeID = getNodeID(instr)
        val nodeInfo = getInfo(instr)
        val (newGraph, newNode) = G.addNode'(curGraph, nodeID, nodeInfo)
      in
        (newGraph, newNode::curNodeList)
      end
  fun createEdges (graph,[]) = (graph, [])
    | createEdges (graph, instr::rest) =
      let
        val (graph, prevNode::rest) = createEdges(graph, rest)
        val curNodeID = getNodeID(instr)
        val prevNodeID = G.getNodeID(prevNode)
        val curNode = G.getNode(graph, curNodeID)
        (* fall through *)
        val graph = G.addEdge(graph, {from=curNodeID, to=prevNodeID})
        (* jump labels *)
        val _ = case instr of A.OPER {assem, dst, src, jump} =>
            (case jump of SOME(labels) =>
            let
              fun addEdgeForOneLabel(label, graph) =
                let
                  val jumpNodeID = S.name label
                in
                  (* exception? *)
                  G.addEdge(graph, {from=curNodeID, to=jumpNodeID})
                end
            in
              foldl addEdgeForOneLabel graph labels
            end
                        | NONE => graph)
            | _ => graph
      in
        (graph, curNode::prevNode::rest)
      end

  fun instrs2graph (instrList) =
    let
      val graph = createEmptyGraph();
      val (graph, nodeList) = createNodes(graph, instrList)
      val _ = resetNodeId()
      val (graph, nodeList) = createEdges(graph, instrList)
    in
      (F.FGRAPH {control=graph}, nodeList)
    end
end
