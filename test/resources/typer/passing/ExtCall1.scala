object Call {
  def foo(n: Int): String => Unit = {
    Std.printString(Std.intToString(n));
    Std.printString
  }
  def bar(f: (Int => String => Unit), n: Int, s: String): Unit = { f(n)(s) }
  bar(foo, 1, "whaaat???");
  bar(foo, 2, "helloooooo!!!");
  bar(foo, 3, "wurrrlllld~~~")
}
