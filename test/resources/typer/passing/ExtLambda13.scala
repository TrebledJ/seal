object Lambda {
  val x: Int = (\() -> (1, ("a", 2, false), true))()(1)(1);
  Std.printInt(x)
}
