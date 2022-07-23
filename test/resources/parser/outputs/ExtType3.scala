object Type3 {

  def g(a: String): Int = {
    0
  }

  def f(a: (String => Int) => Int): Int = {
    a(g)
  }

  0
}