// Requires: Std, File, String.

object Shop {
    abstract class List
    case class Nil() extends List
    case class Cons(h: (String, Int, Int), t: List) extends List

    def read(filename: String): List = {
        val file: File.Reader = File.open(filename);
        readi(file)
    }
    
    def readi(file: File.Reader): List = {
        if (File.isEOF(file)) {
            Nil()
        } else {
            val product: String = File.readString(file);
            val price: Int = File.readInt(file);
            val qty: Int = File.readInt(file);
            Cons((product, price, qty), readi(file))
        }
    }

    def filter(f: ((String, Int, Int)) => Boolean): List => List = {
        \(xs: List) ->
            xs match {
                case Nil() => Nil()
                case Cons(y, ys) => if (f(y)) { Cons(y, filter(f)(ys)) } else { filter(f)(ys) }
            }
    }

    def isEmpty(xs: List): Boolean = {
        xs match {
            case Nil() => true
            case Cons(_, _) => false
        }
    }

    def not(b: Boolean): Boolean = { !b }

    def compose0(g: (List => Boolean), f: (List => List)): (List => Boolean) = { \(x: List) -> g(f(x)) }
    def compose1(g: (Boolean => Boolean), f: (List => Boolean)): (List => Boolean) = { \(x: List) -> g(f(x)) }

    def flip(f: String => List => Boolean): List => String => Boolean = {
        \(x: List) -> \(y: String) -> f(y)(x)
    }

    def inStock(product: String): List => Boolean = {
        compose1(not, 
            compose0(isEmpty, 
                filter(\(prod: (String, Int, Int)) -> Str.equals(prod(0), product))
            ))
    }

    def purchase(product: String, qty: Int, products: List): List = {
        products match {
            case Nil() => Nil()
            case Cons((prod, p, q), rest) =>
                if (Str.equals(prod, product)) {
                    if (q < qty) {
                        error("Not enough " ++ prod ++ " in stock!")
                    } else {
                        Std.printString("Bought " ++ Std.intToString(qty) ++ " " ++ product ++ "!");
                        if (q == qty) {
                            Std.printString("All " ++ prod ++ " have been sold!");
                            rest
                        } else {
                            Cons((prod, p, q - qty), rest)
                        }
                    }
                } else {
                    Cons((prod, p, q), purchase(product, qty, rest))
                }
        }
    }

    val list: List = read("extension-examples/input/fruit.txt");
    (val checker: String => Boolean = flip(inStock)(list);
        Std.printString("Has apples? " ++ Std.booleanToString(checker("apples")));
        Std.printString("Has bananas? " ++ Std.booleanToString(checker("bananas")));
        Std.printString("Has grapes? " ++ Std.booleanToString(checker("grapes")))
    );
    val list1: List = purchase("apples", 5, list);
    // purchase("bananas", 10, list) // Not enough bananas in stock. :(
    val list2: List = purchase("bananas", 6, list1); //
    (val checker: String => Boolean = flip(inStock)(list2);
        Std.printString("Has apples? " ++ Std.booleanToString(checker("apples")));
        Std.printString("Has bananas? " ++ Std.booleanToString(checker("bananas")))
    )
}