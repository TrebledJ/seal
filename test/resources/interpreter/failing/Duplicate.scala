object Duplicate {
  val l1: L.List = L.Nil();
  val l2: L2.List = L2.Nil();
  Std.printString(Std.booleanToString(l1 == l2)) // Typechecker should catch error.
}