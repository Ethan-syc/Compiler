structure Key: ORD_KEY =
struct
  type ord_key = Temp.temp
  val compare = Int.compare
end

structure KeyGraph = FuncGraph(Key)

structure KeySet = BinarySetFn(Key)
