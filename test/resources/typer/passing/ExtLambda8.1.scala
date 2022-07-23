object Lambda {
  val f: Int => Int = \(x: Int) -> x match { case 42 => 1 case _ => 0};
  Std.printInt(f(42));
  Std.printInt(f(41))
}
