package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 20)
    val set2 = set1.incl(a)
    val set3 = set2.incl(b)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    var set6 = new Empty().incl(c).incl(d)
    val e = new Tweet("e", "e body", 50)
    var set7 = set5.incl(e)

  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      val result = set5.filter(tw => tw.user == "a")
      assert(size(result) === 1)
      assert(result.contains(a))
    }
  }

  test("filter: !a on set2") {
    new TestSets {
      val result = set2.filter(tw => tw.user != "a")
      assert(size(result) === 0)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      val result = set5.filter(tw => tw.retweets == 20)
      assert(size(result) === 2)
      assert(result.contains(a))
      assert(result.contains(b))
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      val result = set4c.union(set4d)
      assert(size(result) === 4)
      assert(result.contains(a))
      assert(result.contains(b))
      assert(result.contains(c))
      assert(result.contains(d))
    }
  }

  test("union: set3 to set2") {
    new TestSets {
      val result = set3.union(set2)
      //note: won't check for dupes as size uses set
      assert(size(result) === 2)
    }
  }

  test("union: set5 to self") {
    new TestSets {
      val result = set5.union(set5)
      assert(size(result) === 4)
      assert(result.contains(a))
      assert(result.contains(b))
      assert(result.contains(c))
      assert(result.contains(d))
    }
  }

  test("union: remove from union set4c to 4d") {
    new TestSets {
      val result = set4c.union(set4d)
      assert(size(result) === 4)
      assert(size(result.remove(b)) === 3)
    }
  }

  test("union: remove from union 5d twice") {
    new TestSets {
      assert(size(set5.remove(d)) === 3)
      assert(size(set5.remove(d).remove(c)) === 2)
      val result = set5.remove(d).remove(c)
    }
  }


  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweeted") {
    new TestSets {
      assert(set5.mostRetweeted == a || set5.mostRetweeted == b)
      assert(set6.mostRetweeted === d)
      assert(set7.mostRetweeted === e)
      intercept[NoSuchElementException](set1.mostRetweeted)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: set7") {
    new TestSets {
      val trends = set7.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head === e)
      assert(trends.tail.head == a || trends.tail.head == b)
      assert(trends.tail.tail.head == a || trends.tail.tail.head == b)
      assert(trends.tail.tail.tail.head == d)
      assert(trends.tail.tail.tail.tail.head == c)
      trends.foreach(t => println(t.text))
    }
  }
}
