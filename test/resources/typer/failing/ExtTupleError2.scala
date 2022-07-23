object TupleError {
  def f(a: (Int, String)): Unit = {
    Std.printString(Std.intToString(a(2)));
    Std.printString(a(1))
  }

  f((1, " hello!"))
}