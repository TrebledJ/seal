object TupleError {
  def foo(x: Int, y: String): Boolean = { true }
  val f: Int => ((Int, String)) => Boolean = \(x: Int) -> foo;
  // val g: ((Int, String)) => Boolean = foo; // OK in Scala, error in Seal.
  f(42)(1, "abc")
}