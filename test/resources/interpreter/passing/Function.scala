object Function {
  def add(x: Int, y: Int): Int = { x + y }

  Std.printString(Std.intToString(add(1, 2)));
  Std.printString(Std.intToString(add(1, 3)));
  Std.printString(Std.intToString(add(1, 4)));
  Std.printString(Std.intToString(add(1, -5)))
}