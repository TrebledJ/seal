object TypeError {
  def f(a: Int => String): Int = {
    a(0)
  }
  0
}
