// Requires: Std, File, List, Option.

/**
 * Solves both parts of Advent of Code 2021 Day 1...
 * ...if only the second part could fit into RAM.
 */
object AOC2021D01 {
    def parse(): L.List = {
        // File IO.
        val file: File.Reader = File.open("extension-examples/input/d01.txt");
        parsei(file, L.Nil())
    }

    def parsei(file: File.Reader, list: L.List): L.List = {
        if (File.isEOF(file)) {
            list
        } else {
            parsei(file, L.Cons(File.readInt(file), list))
        }
    }

    def part1(): Unit = {
        val file: File.Reader = File.open("extension-examples/input/d01.txt");
        val first: Int = File.readInt(file);
        val count: Int = part1i(file, first, 0);
        Std.printInt(count)
    }

    def part1i(file: File.Reader, prev: Int, count: Int): Int = {
        if (File.isEOF(file)) {
            count
        } else {
            val cur: Int = File.readInt(file);
            if (prev < cur) {
                part1i(file, cur, count + 1)
            } else {
                part1i(file, cur, count)
            }
        }
    }

    // Higher order functions.
    def zipWith(f: (Int, Int) => Int, l1: L.List, l2: L.List): L.List = {
        l1 match {
            case L.Nil() => L.Nil()
            case L.Cons(h1, t1) =>
                l2 match {
                    case L.Nil() => L.Nil()
                    case L.Cons(h2, t2) => L.Cons(f(h1, h2), zipWith(f, t1, t2))
                }
        }
    }

    def part2(): Unit = {
        // Stack overflows. :(
        val l: L.List = parse();
        val add: (Int, Int) => Int = \(a: Int, b: Int) -> a + b;
        val summed: L.List = l match {
            case L.Cons(_, t1) =>
                t1 match {
                    case L.Cons(_, t2) =>
                        zipWith(add, zipWith(add, l, t1), t2)
                }
        };
        summed match {
            case L.Cons(h, t) =>
                Std.printInt(part2i(t, h, 0))
        }
    }

    def part2i(list: L.List, prev: Int, count: Int): Int = {
        list match {
            case L.Nil() => count
            case L.Cons(h, t) => 
                if (prev < h) {
                    part2i(t, h, count + 1)
                } else {
                    part2i(t, h, count)
                }
        }
    }

    part1()
    // part2()
}