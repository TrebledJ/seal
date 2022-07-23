object TuplePattern {
  val a: (Int, String) = (1, "hi!");
  a match {
    case (_, x) => Std.printString(x)
  }
}