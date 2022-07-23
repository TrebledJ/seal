object Basic {
  Std.printString("Hello " ++ "world!");
  Std.printString(Std.intToString(1 + 2 * 3));
  if (1 == 1) {
    Std.printString("1 == 1!")
  } else {
    Std.printString("1 != 1!")
  };
  if (2 == 1) {
    Std.printString("2 == 1!")
  } else {
    Std.printString("2 != 1!")
  };
  Std.printString("Done!")
}
