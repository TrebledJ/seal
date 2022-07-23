object ParamAndLocal {
  def foo(): Unit => Int = {
    val x: Int = 1;
    val f: () => Int = \() -> x;
    f
  }
  val x: String = "";
  Std.printInt(foo()())
}
