object Tuple {

  def foo(x: (Int, String)): Boolean = {
    true
  }

  val f: Int => ((Int, String)) => Boolean =
    \(x: Int) -> foo;
  f(2)((1, "abc"))
}