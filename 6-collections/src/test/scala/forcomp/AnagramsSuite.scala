package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }



  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: abced e") {
    assert(sentenceOccurrences(List("abced", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 2)))
  }

  test("sentenceOccurrences: abced e Eggbert") {
    assert(sentenceOccurrences(List("abced", "e", "Eggbert")) === List(('a', 1), ('b', 2), ('c', 1), ('d', 1), ('e', 4), ('g', 2), ('r', 1), ('t', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

//  test("dictionaryByOccurrences.get: i") {
//    assert(dictionary.find(w => w == "i").isDefined)
//    assert(dictionaryByOccurrences.get(List(('i', 1))).map(_.toSet) === Some(Set("i")))
//  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: aa - empty") {
    assert(subtract(List(('a', 2)), List()) === List(('a', 2)))
  }

  test("subtract: a - a") {
    assert(subtract(List(('a', 1)), List(('a', 1))) === List())
  }

  test("subtract: lard - rd") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val rd = List(('r', 1), ('d', 1))
    val la = List(('a', 1), ('l', 1))
    assert(subtract(lard, rd) === la)
  }

//  test("dictionary has i") {
//
//    assert(dictionaryByOccurrences.contains(List(('i', 1))))
//  }

  test("subtract: aaaab - aa") {
    assert(subtract(List(('a', 4), ('b', 1)), List(('a', 2))) === List(('a', 2), ('b', 1)))
  }

  test("subtract: abc - ab") {
    assert(subtract(List(('a', 1), ('b', 1), ('c', 1)), List(('a', 1), ('b', 1))) === List(('c', 1)))
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: aa") {
    assert(combinations(List(('a', 2))).toSet === List(List(), List(('a', 1)), List(('a', 2))).toSet)
    assert(combinations(subtract(List(('a', 1)), List(('a', 1)))).toSet === List(List()).toSet)

  }



  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }




  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

//  test("sentence anagrams: a") {
//    val sentence = List("a")
//    val anas = List(
//      List("a")
//    )
//    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
//  }

  test("sentence anagrams: no") {
    val sentence = List("no")
    val anas = List(
      List("no"), List("on")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }  

}
