object Duplicate2 {
  val l: L.List = L.Nil();
  l match {
    case L.Nil() => Std.printString("ok")
  };
  l match {
    case L2.Nil() => Std.printString("bad bad") // Typechecker should catch error.
  }
}