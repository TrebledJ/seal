object Call {
  def foo(s: String): Unit = { Std.printString }
  val f: String => Unit = Std.printString;
  val g: String => Unit = foo;
  f("Hello...");
  g("...world")
}
