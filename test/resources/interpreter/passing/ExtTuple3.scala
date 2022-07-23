object Tuple {
  val a: (Int, String, Boolean) = (1, "a", true);
  Std.printString(Std.intToString(a(0)));
  Std.printString(a(1));
  Std.printString(Std.booleanToString(a(2)))
}