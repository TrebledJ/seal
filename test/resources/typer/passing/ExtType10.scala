object Type {
  def g(a: String): Int = {
    0
  }
  def f(a: (String => Int) => Int): Int = {
    val h: String => Int = g;
    a(h)
  }
  0
}
