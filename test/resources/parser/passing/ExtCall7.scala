object Call {
  def unary(): Unit = {
    Std.printString("yo wassup")
  }
  (unary)();
  val f: () => Unit = unary;
  f()
}
