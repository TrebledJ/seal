object Tuple {
  def foo(x: Int, y: String): Boolean = { Std.printInt(x); Std.printString(y); true }
  val f: Int => (Int, String) => Boolean = \(x: Int) -> (Std.printInt(x); foo);
  // val g: ((Int, String)) => Boolean = foo; // OK in Scala, error in Seal.
  f(42)(1, "abc")
}