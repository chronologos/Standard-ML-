structure SetT = SplaySetFn (struct
  type ord_key = int
  val compare = Int.compare
end)

val a = SetT.addList(SetT.empty, [1,2,3,1,23,3])
val b = SetT.listItems(a)
