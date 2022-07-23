object TupleError {
  def f(a: (Int, String)): Unit = {
    val b: Int = 1;
    Std.printString(a(b))
  }

  f((1, " hello!"))
}