object Tuple {
  def foo(x: (Int, String)): Boolean = { Std.printInt(x(0)); Std.printString(x(1)); true }
  val f: ((Int, String)) => Boolean = foo;
  f((1, "abc"))
  // f(1, "abc") // OK in Scala, error in Seal.
}