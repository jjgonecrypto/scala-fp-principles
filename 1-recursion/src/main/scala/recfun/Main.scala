package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    (c, r) match {
      case _ if c < 0 || c > r => 0 //no defn for these edge cases
      case _ if c == 0 || c == r => 1
      case _ => pascal (c - 1, r - 1) + pascal (c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val lhs = '('
    val rhs = ')'

    def tailAfterClosingBracket(haystack: List[Char]):List[Char] = {
      haystack match {
        case Nil => List(rhs) //no match, so return RHS to act as signal that no match was found
        case head :: tail if head == rhs => tail
        case head :: tail if head == lhs => tailAfterClosingBracket(tailAfterClosingBracket(tail))
        case head :: tail => tailAfterClosingBracket(tail)
      }
    }

    chars match {
      case Nil => true
      case head :: tail if head == rhs => false //any out of band RHS will cause failure
      case head :: tail if head == lhs => balance(tailAfterClosingBracket(tail))
      case _ :: tail => balance(tail)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    coins.sorted match {
      case _ if money == 0 => 0
      case _ if coins == Nil => 0
      case coin :: rest if coin == money => 1
      case coin :: rest if coin > money || coin <= 0 => countChange(money, rest)
      case coin :: rest => countChange(money - coin, coins) + countChange(money, rest)
    }
  }
}
