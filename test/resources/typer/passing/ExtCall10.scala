object Call {
  def f(s: String): (Int, String => Unit) = {
    (0, Std.printString)
  }
  (f("a"))(1)("hello world")
}
