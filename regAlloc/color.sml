signature COLOR =
sig
    structure Frame: FRAME
    type allocation = Frame.register Temp.Table.table
    val color: {interference: KeySet.set KeyGraph.graph,
                initial: allocation,
                spillCost: Temp.temp -> int,
                registers: Frame.register list}
                   -> allocation * Temp.temp list
end

structure Color:COLOR =
struct
exception spill
exception impossible
structure Frame = MipsFrame
type allocation = Frame.register Temp.Table.table
structure K = Key
structure S = KeySet
structure G = KeyGraph
structure L = Liveness
structure T = Temp.Table
(* for chosing color *)
structure colorSet = BinarySetFn(
    struct
    type ord_key = Frame.register
    val compare = String.compare
    end)

fun findNodeToSimplyify([], trivalDegree) = raise impossible
  | findNodeToSimplyify([node], trivalDegree) =
    if (G.inDegree(node)) < trivalDegree then node else (Log.error "found a spill"; raise spill)
  | findNodeToSimplyify(node::rest, trivalDegree) =
    if (G.inDegree(node)) < trivalDegree then node else findNodeToSimplyify(rest, trivalDegree)

fun getOneFreeColor(neighborColorsSet, registers) =
    let
        val allColorSet = colorSet.addList(colorSet.empty, registers)
        val freeColorSet = colorSet.difference(allColorSet, neighborColorsSet)
    in
        hd (colorSet.listItems(freeColorSet))
    end

fun getOptional (SOME color) = color
  (* will raise spill earlier, so this is impossible to happen *)
  | getOptional (NONE) = (Log.error "can't find the color"; raise impossible)

fun color {interference, initial, spillCost, registers} =
    if null (G.nodes(interference)) then (initial, [])
    else
        let
            val trivalDegree = length(registers)
            val nodeToSimplify = findNodeToSimplyify(G.nodes(interference), trivalDegree)
            val neighborIDList = G.succs nodeToSimplify
            val newGraph = G.removeNode (interference, G.getNodeID nodeToSimplify)
            val (allocation, spills) = color {interference=newGraph, initial=initial, spillCost=spillCost, registers=registers}
            val neighborColorsSet = foldl (fn (ID, setSoFar) => colorSet.add(setSoFar, getOptional(T.look(allocation, ID)))) colorSet.empty neighborIDList
            val colorToAssign = getOneFreeColor(neighborColorsSet, registers)
            val newAllocation = T.enter(allocation, G.getNodeID nodeToSimplify, colorToAssign)
        in
            (newAllocation, [])
        end
end
