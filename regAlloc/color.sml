signature COLOR =
sig
    structure Frame: FRAME
    exception spill of Temp.temp
    type allocation = Frame.register Temp.Table.table
    val color: {interference: KeySet.set KeyGraph.graph,
                initial: allocation,
                spillCost: Temp.temp -> int,
                registers: Frame.register list}
               -> allocation * Temp.temp list
end

structure Color:COLOR =
struct
exception spill of Temp.temp
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

fun isPreColored initial temp = isSome(T.look(initial, temp))

fun findNodeToSimplify ([], trivalDegree, initial) = NONE
  | findNodeToSimplify(node::rest, trivalDegree, initial) =
    if isPreColored initial (G.getNodeID node)
    then findNodeToSimplify(rest, trivalDegree, initial)
    else if (G.inDegree(node)) < trivalDegree
    then SOME(node)
    else findNodeToSimplify(rest, trivalDegree, initial)

fun getOneFreeColor(neighborColorsSet, registers) =
    let
        val neighborColorsList = colorSet.listItems(neighborColorsSet)
        val freeColorList = foldr (fn (color, curResult) =>
                                      let
                                          val used = colorSet.member(neighborColorsSet, color)
                                      in
                                          if used then curResult else color::curResult
                                      end
                                  ) [] registers
    in
        hd freeColorList
    end

fun getOptional (SOME color) = color
  (* will raise spill earlier, so this is impossible to happen *)
  | getOptional (NONE) = (Log.error "can't find the color"; raise impossible)

fun color {interference, initial, spillCost, registers} =
    let
        val trivalDegree = length(registers)
        val nodes = G.nodes(interference)
        val nodeToSimplify = findNodeToSimplify(nodes, trivalDegree, initial)
        val newAllocation =
            case nodeToSimplify
             of NONE =>
                let
                    val temps = map G.getNodeID nodes
                    val allPreColored = Utils.allOf (map (isPreColored initial) temps)
                    val result = if allPreColored then initial
                                 else
                                     let val min = Utils.min spillCost temps
                                     in
                                         raise spill min
                                     end
                in
                    result
                end
              | SOME(nodeToSimplify) =>
                let
                    val neighborIDList = G.succs nodeToSimplify
                    val newGraph = G.removeNode (interference, G.getNodeID nodeToSimplify)
                    val (allocation, spills) = color {interference=newGraph,
                                                      initial=initial,
                                                      spillCost=spillCost,
                                                      registers=registers}
                    val neighborColorsSet = foldl (fn (ID, setSoFar) =>
                                                      colorSet.add(setSoFar,
                                                                   getOptional(T.look(allocation, ID)))) colorSet.empty neighborIDList
                    val colorToAssign = getOneFreeColor(neighborColorsSet, registers)
                    val newAllocation = T.enter(allocation, G.getNodeID nodeToSimplify, colorToAssign)
                in
                    newAllocation
                end
    in
        (newAllocation, [])
    end
end
