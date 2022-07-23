object L2 {
  def filterLeq(n: Int, l: L.List): L.List = {
    l match {
      case L.Nil() => L.Nil()
      case L.Cons(x, xs) => if (x <= n) { L.Cons(x, filterLeq(n, xs)) } else { filterLeq(n, xs) }
    }
  }

  def filterGt(n: Int, l: L.List): L.List = {
    l match {
      case L.Nil() => L.Nil()
      case L.Cons(x, xs) => if (!(x <= n)) { L.Cons(x, filterGt(n, xs)) } else { filterGt(n, xs) }
    }
  }

  def quickSort(l: L.List): L.List = {
    l match {
      case L.Nil() => L.Nil()
      case L.Cons(x, xs) => // Use x as pivot.
        L.concat(quickSort(filterLeq(x, xs)), L.Cons(x, quickSort(filterGt(x, xs))))
    }
  }

  def mean(l: L.List): Int = {
    L.sum(l) / L.length(l)
  }

  def max(l: L.List): Int = {
    l match {
      case L.Nil() => error("Max on empty list")
      case L.Cons(x, L.Nil()) => x
      case L.Cons(x, xs) => Ext.max(x, max(xs))
    }
  }

  def min(l: L.List): Int = {
    l match {
      case L.Nil() => error("Min on empty list")
      case L.Cons(x, L.Nil()) => x
      case L.Cons(x, xs) => Ext.min(x, min(xs))
    }
  }

  val l: L.List = L.Cons(299, L.Cons(1721, L.Cons(979, L.Cons(366, L.Cons(299, L.Cons(675, L.Cons(1456, L.Nil())))))));
  Std.printString("quick sorted: " ++ L.toString(quickSort(l)));
  Std.printString("merge sorted: " ++ L.toString(L.mergeSort(l)));
  Std.printString("mean: " ++ Std.intToString(mean(l)));
  Std.printString("min: " ++ Std.intToString(min(l)));
  Std.printString("max: " ++ Std.intToString(max(l)));
  
  0
}