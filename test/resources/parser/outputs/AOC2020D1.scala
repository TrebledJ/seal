object AOC2020D1 {

  def part1(l: L.List, target: Int): Unit = {
    part1go(l, target) match {
      case O.None() =>
        Std.printString((("No two numbers sum to " ++ Std.intToString(target)) ++ ". :("))
      case O.Some(x) =>
        (
          Std.printString((((("Numbers: " ++ Std.intToString(x)) ++ " ") ++ Std.intToString((target - x))) ++ "."));
          Std.printString(("Answer: " ++ Std.intToString((x * (target - x)))))
        )
    }
  }

  def part1go(l: L.List, target: Int): O.Option = {
    l match {
      case L.Nil() =>
        O.None()
      case L.Cons(x, xs) =>
        (if(L.contains(xs, (target - x))) {
          O.Some(x)
        } else {
          part1go(xs, target)
        })
    }
  }

  def part2(l: L.List, target: Int): Unit = {
    part2go(l, target) match {
      case L.Nil() =>
        Std.printString((("No three numbers sum to " ++ Std.intToString(target)) ++ ". :("))
      case L.Cons(x, L.Cons(y, L.Cons(z, L.Nil()))) =>
        (
          Std.printString(((((("Numbers: " ++ Std.intToString(x)) ++ " ") ++ Std.intToString(y)) ++ " ") ++ Std.intToString(z)));
          Std.printString(("Answer: " ++ Std.intToString(((x * y) * z))))
        )
    }
  }

  def part2go(l: L.List, target: Int): L.List = {
    l match {
      case L.Nil() =>
        L.Nil()
      case L.Cons(x, xs) =>
        part1go(xs, (target - x)) match {
          case O.None() =>
            part2go(xs, target)
          case O.Some(y) =>
            (
              val z: Int =
                ((target - x) - y);
              L.Cons(x, L.Cons(y, L.Cons(z, L.Nil())))
            )
        }
    }
  }

  val target: Int =
    2020;
  val l: L.List =
    L.Cons(1721, L.Cons(979, L.Cons(366, L.Cons(299, L.Cons(675, L.Cons(1456, L.Nil()))))));
  part1(l, target);
  val l2: L.List =
    L.Cons(2008, L.Cons(1529, L.Cons(1594, L.Cons(1422, L.Cons(1518, L.Cons(1278, L.Cons(1553, L.Cons(1563, L.Cons(1911, L.Cons(1799, L.Cons(1554, L.Cons(1247, L.Cons(1256, L.Cons(1558, L.Cons(483, L.Cons(1228, L.Cons(1931, L.Cons(1915, L.Cons(1982, L.Cons(1823, L.Cons(2003, L.Cons(1894, L.Cons(1388, L.Cons(1925, L.Cons(1501, L.Cons(1236, L.Cons(1897, L.Cons(1245, L.Cons(1386, L.Cons(1967, L.Cons(1806, L.Cons(1861, L.Cons(1837, L.Cons(1895, L.Cons(207, L.Cons(2002, L.Cons(1688, L.Cons(1214, L.Cons(1464, L.Cons(1905, L.Cons(1936, L.Cons(1926, L.Cons(899, L.Cons(245, L.Cons(1589, L.Cons(1449, L.Cons(1190, L.Cons(1332, L.Cons(1444, L.Cons(2000, L.Cons(1210, L.Cons(1979, L.Cons(1472, L.Cons(1477, L.Cons(1904, L.Cons(1354, L.Cons(1930, L.Cons(1318, L.Cons(1981, L.Cons(1929, L.Cons(1763, L.Cons(1840, L.Cons(1536, L.Cons(1862, L.Cons(1262, L.Cons(1202, L.Cons(1993, L.Cons(1963, L.Cons(1872, L.Cons(1907, L.Cons(1287, L.Cons(1231, L.Cons(1387, L.Cons(1555, L.Cons(1782, L.Cons(1301, L.Cons(1468, L.Cons(1476, L.Cons(1908, L.Cons(1436, L.Cons(1941, L.Cons(1252, L.Cons(1824, L.Cons(1910, L.Cons(1817, L.Cons(1818, L.Cons(131, L.Cons(1201, L.Cons(1869, L.Cons(1357, L.Cons(1983, L.Cons(1543, L.Cons(1836, L.Cons(1860, L.Cons(1648, L.Cons(1916, L.Cons(1825, L.Cons(1875, L.Cons(1233, L.Cons(1289, L.Cons(1071, L.Cons(1355, L.Cons(1761, L.Cons(1846, L.Cons(1392, L.Cons(1966, L.Cons(1204, L.Cons(1906, L.Cons(1830, L.Cons(1309, L.Cons(1427, L.Cons(1347, L.Cons(1315, L.Cons(1602, L.Cons(1323, L.Cons(1461, L.Cons(313, L.Cons(1841, L.Cons(1857, L.Cons(1741, L.Cons(1663, L.Cons(1947, L.Cons(1600, L.Cons(1954, L.Cons(1974, L.Cons(1922, L.Cons(1884, L.Cons(1844, L.Cons(1463, L.Cons(1777, L.Cons(1720, L.Cons(1888, L.Cons(1874, L.Cons(1435, L.Cons(1816, L.Cons(1268, L.Cons(1901, L.Cons(1945, L.Cons(1548, L.Cons(1794, L.Cons(1886, L.Cons(1580, L.Cons(1746, L.Cons(1958, L.Cons(1495, L.Cons(1821, L.Cons(1538, L.Cons(1937, L.Cons(584, L.Cons(1637, L.Cons(1185, L.Cons(1540, L.Cons(1959, L.Cons(1595, L.Cons(1523, L.Cons(1919, L.Cons(1644, L.Cons(1478, L.Cons(1822, L.Cons(1502, L.Cons(1466, L.Cons(1617, L.Cons(1258, L.Cons(267, L.Cons(1855, L.Cons(1887, L.Cons(1471, L.Cons(1632, L.Cons(1726, L.Cons(1415, L.Cons(1424, L.Cons(841, L.Cons(1410, L.Cons(1393, L.Cons(1293, L.Cons(1927, L.Cons(1934, L.Cons(1923, L.Cons(1848, L.Cons(1847, L.Cons(1968, L.Cons(1371, L.Cons(1497, L.Cons(1751, L.Cons(1851, L.Cons(1882, L.Cons(1250, L.Cons(1953, L.Cons(2004, L.Cons(1420, L.Cons(1662, L.Cons(1519, L.Cons(1679, L.Cons(1369, L.Cons(1964, L.Cons(1642, L.Cons(1809, L.Cons(1609, L.Cons(1517, L.Cons(1175, L.Nil()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
  part1(l2, target);
  part2(l, target);
  part2(l2, target);
  0
}