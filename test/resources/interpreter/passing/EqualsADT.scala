object EqualsADT {
  Std.printString("Nil() == Nil(): " ++ Std.booleanToString(L.Nil() == L.Nil()));
  val a: L.List = L.Nil();
  Std.printString("adt == adt: " ++ Std.booleanToString(a == a));
  L.Nil()
}