object EqualsLiteral {
  Std.printString("1 == 1: " ++ Std.booleanToString(1 == 1));
  Std.printString("1 == 2: " ++ Std.booleanToString(1 == 2));
  Std.printString("3 == 1 + 2: " ++ Std.booleanToString(3 == 1 + 2));
  Std.printString("true == true: " ++ Std.booleanToString(true == true));
  Std.printString("false == false: " ++ Std.booleanToString(false == false));
  Std.printString("true == false: " ++ Std.booleanToString(true == false));
  Std.printString("() == (): " ++ Std.booleanToString(() == ()));
  Std.printString("'a' == 'a': " ++ Std.booleanToString("a" == "a"));

  val s: String = "abc";
  Std.printString("s == s: " ++ Std.booleanToString(s == s));
  
  // val s2: String = s;
  // Std.printString("s == s2: " ++ Std.booleanToString(s == s2));
  
  Std.printString("Test == done.")
}