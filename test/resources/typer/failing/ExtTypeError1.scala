object TypeError {
  def f(a: Int => Int): Int = {
    val a: Int = 1;
    a(0)
  }
  0
}
