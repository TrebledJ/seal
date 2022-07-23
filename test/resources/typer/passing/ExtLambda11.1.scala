object Lambda {
  val x: String = "";
  val f: Int => Int = \(x: Int) -> (
    val y: Int = x;
    val x: Boolean = true;
    if (x) { y } else { -y }
  );
  Std.printInt(f(1))
}
