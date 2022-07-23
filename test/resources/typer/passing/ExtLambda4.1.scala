object Lambda {
  val f: Unit => Int = (\() -> 1);
  Std.printInt(f())
}
