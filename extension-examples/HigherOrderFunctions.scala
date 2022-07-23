// Requires: Std, List, Option.

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
        val res: Int => Int = \(x: Int) -> x + i; // Closure converted.
        res
    }

    def add(a: Int): Int => Int = {
        \(b: Int) -> a + b
    }

    def bar(x: (Int, Int)): (Int, Int) = {
        (x(0) + 1, x(1))
    }

    def baz(x: Int, y: Int): (Int, Int) = {
        (x + 1, y)
    }

    val i: Int = 2;
    val l: L.List = map( \(x: Int) -> x + 1, L.Cons(1, L.Cons(2, L.Cons(3, L.Nil()))) );
    val l2: L.List = map(add(10), L.Cons(1, L.Cons(2, L.Cons(3, L.Nil()))) );
    Std.printString(L.toString(l)); // List(2, 3, 4).
    Std.printString(L.toString(l2)); // List(11, 12, 13).
    Std.printInt(compose(\(x: Int) -> x + 1, \(y: Int) -> y * 2)(5)); // 11.
    Std.printInt(foo()(42)); // 43 (captures i = 1).
    Std.printInt((\(x: Int) -> x + i)(42)); // 44 (uses i = 2).

    // \(x: Int) -> val y: Int = 1; x + y; // Error: can't begin lambda body with `val`.
    \(x: Int) -> (val y: Int = 1; x + y); // OK.

    (\(x: Int): Int -> x + 1)(1); // Return type optional.

    // Currying.
    val f: Int => Int => Int = add;
    Std.printInt(add(40)(2));

    val g: ((Int, Int)) => (Int, Int) = bar; // Wrap extra pair of () to for unary functions taking tuples.
    val h: (Int, Int) => (Int, Int) = baz;
    // val h2: ((Int, Int)) => (Int, Int) = baz; // OK in Scala, error in Seal.
    // bar(1, 2); // OK in Scala, error in Seal.
    val t: (Int, Int) = g((3, 1));
    val t2: (Int, Int) = h(3, 0);
    Std.printString(Std.intToString(t(0)) ++ Std.intToString(t(1))); // 41
    Std.printString(Std.intToString(t2(0)) ++ Std.intToString(t2(1))) // 40
}