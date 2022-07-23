object Lambda {
  def g(z: Boolean): Int = {
    val f: Int => Int = \(x: Int): Int -> (
      val y: Int = x;
      if (z) { y * x } else { -y * x }
    );
    f(1)
  }
  val x: String = "";
  Std.printInt(g(true));
  Std.printInt(g(false))
}
