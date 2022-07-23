object Tuple {

  def foo(x: Int, y: String): Boolean = {
    true
  }

  val f: Int => (Int, String) => Boolean =
    \(x: Int) -> foo;
  f(42)(1, "abc")
}