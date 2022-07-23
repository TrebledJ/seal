object TypeError {
  def f(a: (, Int) => Int): Int = {
    a(0)
  }
  0
}
