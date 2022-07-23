object TypeError {
  def f(a: Int => Int): Int = {
    val a: String => Unit = Std.printString;
    a(0)
  }
  0
}
