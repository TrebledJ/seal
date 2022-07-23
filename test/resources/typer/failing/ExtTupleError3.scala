object TupleError {
  def f(a: (Int, String)): Unit = {
    Std.printString(Std.intToString(a(0)));
    Std.printString(a(3))
  }

  f((1, " hello!"))
}