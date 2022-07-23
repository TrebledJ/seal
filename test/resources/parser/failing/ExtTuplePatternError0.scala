object TuplePattern {
  val a: (Int, String) = (1, "hi!");
  a match {
    case (, x) => Std.printString(x)
  }
}