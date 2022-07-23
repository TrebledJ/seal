object Tuple {

  def foo(x: Int, y: String): Boolean = {
    true
  }

  val f: (Int, String) => Boolean =
    foo;
  f(1, "abc")
}