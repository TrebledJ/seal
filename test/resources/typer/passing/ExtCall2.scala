object Call {
  def foo(n: Int): String => Unit = {
    Std.printString(Std.intToString(n));
    Std.printString
  }
  val f: Int => String => Unit = foo;
  f(0)("test")
}
