object Lambda {
  val x: String = "a";
  val y: Int = 3;
  val f: (Int, Int) => Int = (\(x: Int, z: Int) -> (val x: Int = 2; x + y + z));
  Std.printInt(f(1, 10))
}
