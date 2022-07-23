object Tuple {
  def foo(x: Int, y: String): (((Int, Boolean))) = { Std.printInt(x); Std.printString(y); (0, true) }
  val f: Int => (Int, String) => (((((((Int)), (Boolean)))))) = \(x: Int) -> (Std.printInt(x); foo);
  // val g: ((Int, String)) => Boolean = foo; // OK in Scala, error in Seal.
  val a: (Int, Boolean) = f(42)(1, "abc");
  Std.printInt(a(0));
  Std.printBoolean(a(1))
}