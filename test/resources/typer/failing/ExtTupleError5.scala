object TupleError {
  def f(a: (Int, String)): Unit = {
    Std.printString((a(1), a(0))(1))
  }

  f((1, " hello!"))
}