object TypeError {
  def f(a: => Int): Int = {
    a(0)
  }
  0
}
