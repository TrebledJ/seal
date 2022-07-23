// Requires: Std.

object Tuples {
    def maybeNeg(v: (Int, Boolean)): (Int, Boolean) = { // Type.
        v match {
            case (i, false) => // Pattern.
                (i, false) // Literal.
            case (i, true) =>
                (-i, false)
        }
    }

    val a: (Int, Boolean) = maybeNeg((1, true));
    val b: (Int, Boolean) = maybeNeg((1, false));
    val a0: Int = a(0); // Variable access.
    Std.printString("a: (" ++ Std.intToString(a0) ++ ", " ++ Std.booleanToString(a(1)) ++ ")");
    Std.printString("b: (" ++ Std.intToString(b(0)) ++ ", " ++ Std.booleanToString(b(1)) ++ ")");
    
    // val a1: Int = a(0 + 1); // Error: only int literals allowed as tuple accessors.
    // val a2: Int = a(2); // Error: index out of range.

    val c: String = ("hello", "there")(0); // Literal access.
    Std.printString(c)
}