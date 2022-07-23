object Tuple {

  def foo(x: Int, y: String): (Int, Boolean) = {
    true
  }

  val f: Int => (Int, String) => (Int, Boolean) =
    \(x: Int) -> foo;
  f(42)(1, "abc")
}