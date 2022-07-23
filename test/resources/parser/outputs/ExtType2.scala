object Type2 {

  def f(a: Int => String => Int): String => Int = {
    a(0)
  }

  0
}