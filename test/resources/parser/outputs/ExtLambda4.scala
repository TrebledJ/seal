object Lambda {

  val f: Unit => Int =
    \(): Int -> 1;
  Std.printInt(f())
}