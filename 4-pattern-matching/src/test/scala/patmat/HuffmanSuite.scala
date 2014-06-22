package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    )
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
      assert(weight(sampleTree) === 4)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
      assert(chars(sampleTree) === List('x' ,'e', 't'))
    }
  }

  test("times examples") {
    new TestTrees {
      assert(times("".toList) === List())
      assert(times("alalabba".toList) === List(('a', 4), ('l', 2), ('b', 2)))
      assert(times(" bubba gump!".toList) ===
        List((' ', 2), ('b', 3), ('u', 2), ('a', 1), ('g', 1), ('m', 1), ('p', 1), ('!', 1)))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("ordered combine") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 4), Leaf('x', 5))
    assert(combine(leaflist) === List(Leaf('x',5), Fork(Leaf('e',2),Leaf('t',4),List('e', 't'),6)))
  }

  test("createCodeTree") {
    createCodeTree("alibaba".toList).toString()
  }

  test("simple decode") {
    new TestTrees {
      assert(decode(t1, List(0,1)).mkString === "ab")
    }
  }

  test("decodeSecret") {
    assert(decodedSecret.mkString === "huffmanestcool")
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode t1 and t2") {
    new TestTrees {
      assert(encode(t1)("ab".toList).mkString === "01")
      assert(encode(t2)("dab".toList).mkString === "10001")
    }
  }

  test("encode secret") {
    assert(encode(frenchCode)("huffmanestcool".toList) === secret)
  }

  test("quickEncode secret") {
    assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
  }

}
