object Call {
    def f(a: Int): Int = {
        a + 1
    }
    (f(1) == f(2)) && (0 == f(3) + f(4) - f(5 * 6) / 
    f(1 match { case 0 => 1 case 1 => 2 }))
}