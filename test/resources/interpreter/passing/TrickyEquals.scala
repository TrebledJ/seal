object TrickyEquals {
  // True.
  val s: String = "haha!";
  Std.printString(Std.booleanToString(s == s));
  Std.printString(Std.booleanToString(!(s == "haha")));

  val s1: String = s;
  Std.printString(Std.booleanToString(s1 == s));
  
  val n: Int = 1;
  Std.printString(Std.booleanToString(1 == 1)); // Literals.
  Std.printString(Std.booleanToString(n == n)); // Variable names.
  Std.printString(Std.booleanToString(n == 3 - 2)); // Mixture.
  Std.printString(Std.booleanToString(3 - 2 == n));
  Std.printString(Std.booleanToString(3 == n + 2));
  
  val l: L.List = L.Nil();
  Std.printString(Std.booleanToString(l == l));

  // False.
  Std.printString("=========");
  Std.printString(Std.booleanToString(3 == n + 1));
  Std.printString(Std.booleanToString(3 == 1));
  Std.printString(Std.booleanToString(3 == n));
  Std.printString(Std.booleanToString("abc" == "haha"));
  Std.printString(Std.booleanToString("abc" == "abc"));
  Std.printString(Std.booleanToString("abc" == "a" ++ "bc"));
  Std.printString(Std.booleanToString(s == "abc"));
  Std.printString(Std.booleanToString(L.Nil() == L.Nil()));
  Std.printString(Std.booleanToString(L.Nil() == l))
}