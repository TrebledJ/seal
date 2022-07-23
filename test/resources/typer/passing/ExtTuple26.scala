object Tuple {
  def foo(x: Int, y: String): (((Int, Boolean))) = { (0, true) }
  val f: Int => (Int, String) => (((((((Int)), (Boolean)))))) = \(x: Int) -> foo;
  // val g: ((Int, String)) => Boolean = foo; // OK in Scala, error in Seal.
  f(42)(1, "abc")
}