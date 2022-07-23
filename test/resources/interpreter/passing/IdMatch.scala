object IdMatch {
  val x: Int = 5;
  val y: Int = x match {
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case v => v + 1
  };
  Std.printString(Std.intToString(y))
}