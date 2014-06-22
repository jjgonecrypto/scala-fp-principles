val x = List(('a', 4), ('b', 1))
val y = List(('a', 2), ('b', 1))

val result = (x ++ y).groupBy { case(a:Char, i:Int) => a }.toList.sortBy(_._1)

//result.foldLeft(0) {case (acc, List(_, cur)) => cur - acc }


(for {
  (c, list) <- (x ++ y).groupBy { case(a:Char, i:Int) => a }
  i = list.foldLeft(0) {case (acc, (_, cur)) => Math.abs(cur - acc) }
  if i > 0
} yield (c, i))