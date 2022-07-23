object IntMatch {
  val x: Int = 1;
  val y: Int = x match {
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case _ => -1
  };
  Std.printString(Std.intToString(y))
}