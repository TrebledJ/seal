object Tuple {
  def f(a: (Int, String)): Unit = {
    val b: (String, Int) = (a(1), a(0));
    Std.printString(Std.intToString(b(1)));
    Std.printString(b(0))
  }

  f((1, " hello!"))
}