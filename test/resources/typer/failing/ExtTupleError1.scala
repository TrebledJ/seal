object TupleError {
  def f(a: (Int, String)): Unit = {
    Std.printString(a(0));
    Std.printString(a(1))
  }

  f((1, " hello!"))
}