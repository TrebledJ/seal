object Tuple {
  def foo(x: (Int, String)): Boolean = { true }
  val f: ((Int, String)) => Boolean = foo;
  f(1, "abc") // OK in Scala, error in Seal.
}