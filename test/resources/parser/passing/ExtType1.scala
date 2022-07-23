object Type1 {
  def f(a: (String, Int) => Int): Int = {
    a("abc", 0)
  }
  0
}
