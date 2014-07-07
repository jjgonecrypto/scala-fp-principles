package streams

import org.mockito.Matchers
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import org.mockito.Mockito._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite with MustMatchers  {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11 => OOB")
      assert(!terrain(Pos(4,3)), "4,3 => -")
      assert(!terrain(Pos(-1, 1)), "-1,1")
      assert(terrain(Pos(1, 1)), "1,1 => S")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4, 7))
    }
  }

  test("moving and legality") {
    new Level1 {
      assert(startBlock.down.isLegal, "1D OK")
      assert(!startBlock.down.down.isLegal, "2D Bad")
      assert(!startBlock.left.isLegal, "1L Bad")
      assert(startBlock.right.isLegal, "1R OK")
      assert(!startBlock.right.up.isLegal, "1R1U Bad")
    }
  }

  test("moving and standing") {
    new Level1 {
      assert(startBlock.isStanding, "Standing at start")
      assert(!startBlock.down.isStanding, "1D No stand")
      assert(startBlock.down.right.up.isStanding, "1D1R1U Stand")
    }
  }

  test("neighborsWithHistory") {
    import Bloxorz.Level1._
    assert(neighborsWithHistory(startBlock, List(Left,Up)).toSet ===
      Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
    )
  }

  test("newNeighborsOnly") {
    import Bloxorz.Level1._
    assert(newNeighborsOnly(
      Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream,
      Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
    ).toSet === Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))
    )
  }

  test("from: level0") {
    import Bloxorz.Level0._
    assert(from(Stream((startBlock, List())), Set(startBlock)).toSet ===
      Set(
        (Block(Pos(2,2),Pos(3,2)),List(Down)),
        (Block(Pos(2,3),Pos(3,3)),List(Right, Down)),
        (Block(Pos(1,3),Pos(1,3)),List(Up, Right, Down))
      )
    )
  }

  test("pathsFromStart: level0") {
    import Bloxorz.Level0._
    assert(pathsFromStart.toSet ===
      Set(
        (Block(Pos(2,2),Pos(3,2)),List(Down)),
        (Block(Pos(2,3),Pos(3,3)),List(Right, Down)),
        (Block(Pos(1,3),Pos(1,3)),List(Up, Right, Down))
      )
    )
  }

  test("pathsToGoal: level0") {
    import Bloxorz.Level0._
    assert(pathsToGoal.toSet ===
      Set(
        (Block(Pos(1,3),Pos(1,3)),List(Up, Right, Down))
      )
    )
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
