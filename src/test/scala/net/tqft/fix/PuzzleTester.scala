package net.tqft.fix

import scala.sys.process._

object PuzzleTester extends App {
  val n = if(args.size > 0) {
    args(0).toInt
  } else {
    3
  }
  
  val emptyBoard = Puzzle(Board.empty(Seq(n,n)), Seq.empty)
  var difficulty = -1
  while (true) {
    val puzzle = emptyBoard.addPuzzlePiecesUntilUnique.minimize.next.clueMinimize.next.shuffle
    val puzzleDifficulty = puzzle.difficulty
    println(puzzleDifficulty)
    if (puzzleDifficulty > difficulty * 0.65) {
      if (puzzleDifficulty > difficulty) {
        difficulty = puzzleDifficulty
      }
      println(puzzle)
      ("open " + RenderBoard.writePuzzlePDF(puzzle)().toString).!!
    }
  }
}