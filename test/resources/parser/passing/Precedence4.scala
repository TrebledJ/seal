object P4 {
    1 + 2 match { case 0 => 0 };
    1 == 1 match { case true => 0 };
    "1" ++ "2" match { case "1" => 0 };
    val a: Int = 1;
    -a match { case 1 => 0 };
    1 * 2 + 3 % 2 == 3 match { case true => 0 };
    (val x: String = "123"; x ++ "456") match { case "123456" => 0 }
}