package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  import FunSets._


  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s_1 = singletonSet(-1)
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val union01 = union(s0, s1)
    val union12 = union(s1, s2)
    val union23 = union(s2, s3)
    val union123 = union(union12, union23)
    val union1234 = union(union123, singletonSet(4))
    val union0123 = union(union123, s0)

    val s1001 = singletonSet(1001)
    val s_1001 = singletonSet(-1001)
    val boundaryUnion = union(s2, union(s1001, s_1001))

    def createSet(list: List[Int]): Set =
      list.foldLeft((y:Int) => false) {(a:Set, b:Int) => union(a, singletonSet(b))}

  }

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  test("singletonSet(1) contains 1 and no others") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
      assert(!contains(s1, 0), "Singleton")
    }
  }

  test("union is implemented") {
    new TestSets {
      assert(!contains(union12, 0), "Union ! 0")
      assert(contains(union12, 1), "Union 1")
      assert(contains(union12, 2), "Union 2")
      assert(!contains(union12, 3), "Union ! 3")
    }
  }

  test("intersection is implemented") {
    new TestSets {
      val i = intersect(union01, union12)
      assert(!contains(i, 0), "Intersection ! 0")
      assert(contains(i, 1), "Intersection 1")
      assert(!contains(i, 2), "Intersection 2")

      val i2 = intersect(s0, union123)
      assert(!contains(i2, 0))
      assert(!contains(i2, 1))
      assert(!contains(i2, 2))
      assert(!contains(i2, 3))
    }
  }

  test("diff is implemented") {
    new TestSets {
      val d = diff(union01, union12)
      assert(contains(d, 0), "Diff 0")
      assert(!contains(d, 1), "Diff 1")
      assert(!contains(d, 2), "Diff 2")

      val d2 = diff(s1, union123)
      assert(!contains(d2, 1))
      assert(!contains(d2, 2))
      assert(!contains(d2, 3))
    }
  }

  test("filter is implemented") {
    new TestSets {
      val f = filter(union1234, x => x % 2 == 0)
      assert(!contains(f, 1))
      assert(contains(f, 2))
      assert(!contains(f, 3))
      assert(contains(f, 4))
    }
  }

  test("forall is implemented") {
    new TestSets {
      assert(forall(union1234, f => f > 0), "Forall 1-4 > 0")
      assert(!forall(union0123, f => f > 0), "Forall 0-4 ! > 0")
      assert(forall(s2, f => f % 2 == 0), "Forall 2 % 2 == 0")
      assert(!forall(union1234, f => f % 2 == 0), "! Forall 1-4 % 2 == 0")
      assert(!forall(union1234, f => !(f % 2 == 0)), "! Forall 1-4 % 2 != 0")

      //boundary test
      assert(forall(boundaryUnion, f => f % 2 == 0), "Forall ignores out of bounds")
      assert(!forall(createSet(List(999, 1000, 1001)), f => f < 1000),
        "Forall counts upper bounds")
      assert(forall(createSet(List(-1001, -1000, -999, 1000, 1001)), f => f > -1001),
        "Forall counts lower bounds")

    }
  }

  test("exists is implemented") {
    new TestSets {
      assert(exists(union1234, f => f % 2 == 0))
      assert(!exists(union1234, f => f < 0))
      assert(exists(union0123, f => f <= 0))

      assert(!exists(boundaryUnion, f => f % 2 == 1), "Exists ignores out of bounds")
    }
  }

  test("map is implemented") {
    new TestSets {
      val m1 = map(union0123, x => x * x)
      assert(contains(m1, 0))
      assert(contains(m1, 1))
      assert(contains(m1, 4))
      assert(contains(m1, 9))
      assert(!contains(m1, 2))
      assert(!contains(m1, 3))
      assert(forall(m1, x => contains(union0123, Math.sqrt(x).toInt)), "Map 1 via Forall")

      val m2 = map(union1234, x => x % 2)
      assert(FunSets.toString(m2) === FunSets.toString(createSet(List(0,1))), "Map 2 via Forall")

      val m3 = map(boundaryUnion, x => x * x)
      assert(forall(m3, x => x == 4), "Map ignores out of bounds")

      val boundsTest = createSet(List(-1001, -1000, 1,3,4,5,7,1000, 1001))
      assert(FunSets.toString(map(boundsTest, f => f - 1)) ===
        FunSets.toString(createSet(List(-1001, 0,2,3,4,6,999))))
    }
  }
}
