object Call {
  def f(s: String): Unit = {
    Std.printString("teehee-- " ++ s)
  }
  ((Std.printString, f)(0))("hello world");
  ((Std.printString, f)(1))("hello world")
}
