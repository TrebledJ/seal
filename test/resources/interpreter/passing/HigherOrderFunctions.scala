object HOF {
    def compose(f: Int => Int, g: Int => Int): Int => Int = {
        \(x: Int) -> f(g(x))
    }
    
    def map(f: Int => Int, l: L.List): L.List = {
        l match {
            case L.Nil() => L.Nil()
            case L.Cons(h, t) => L.Cons(f(h), map(f, t))
        }
    }
    
    def foo(): Int => Int = {
        val i: Int = 1;
        val res: Int => Int = \(x: Int) -> x + i;
        res
    }

    val i: Int = 2;
    val l: L.List = map( \(x: Int) -> x + 1, L.Cons(1, L.Cons(2, L.Cons(3, L.Nil()))) );
    Std.printString(L.toString(l)); // List(2, 3, 4)
    Std.printInt(compose(\(x: Int) -> x + 1, \(y: Int) -> y * 2)(5)); // 11
    Std.printInt(foo()(42)); // 43
    Std.printInt((\(x: Int) -> x + i)(42)) // 44
}