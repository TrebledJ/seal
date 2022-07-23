object TuplePattern {

  def f(a: (Int, Boolean)): (Int, Boolean) = {
    a match {
      case (i, false) =>
        (i, false)
      case (i, true) =>
        (-(i), false)
    }
  }

  def print(a: (Int, Boolean)): Unit = {
    a match {
      case (i, b) =>
        Std.printString(((Std.intToString(i) ++ " ") ++ Std.booleanToString(b)))
    }
  }

  print(f((1, true)));
  print(f((2, false)))
}