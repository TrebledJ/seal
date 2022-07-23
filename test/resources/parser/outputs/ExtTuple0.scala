object Tuple {

  def f(a: (Int, String)): Unit = {
    Std.printString(Std.intToString(a(0)));
    Std.printString(a(1))
  }

  f((1, " hello!"))
}