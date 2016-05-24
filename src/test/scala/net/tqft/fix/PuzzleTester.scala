package net.tqft.fix

import scala.sys.process._

object PuzzleTester extends App {
  val n = if(args.size > 0) {
    args(0).toInt
  } else {
    6
  }
  
//  def completeBoard(n: Int) = {
//    val states= Seq(None) ++ Seq.tabulate(n)(Some(_))
//    Board(Seq(n+1, n+1), (for(s1<- states; s2 <- states) yield Partial(Seq(s1,s2))).toIndexedSeq)
//  }
//  ("open " + RenderBoard.writeBoardPDF(completeBoard(4))().toString).!!
  
  val emptyBoard = Puzzle(Board.empty(Seq(n,n)), Seq.empty)
  var difficulty = -1
  while (true) {
    var puzzle = emptyBoard.addPuzzlePiecesUntilUnique
    println("!")
     puzzle = puzzle.minimize
    println("!")
     puzzle = puzzle.clueMinimize
    println("!")
     puzzle = puzzle.amalgamate.shuffle
    println(s"found a puzzle with ${puzzle.puzzlePieces.size} pieces")
    val puzzleDifficulty = puzzle.difficulty
    println(puzzleDifficulty)
//    if (puzzleDifficulty > difficulty * 0.65) {
      if (puzzleDifficulty > difficulty) {
        difficulty = puzzleDifficulty
      }
      println(puzzle)
//      ("open " + RenderBoard.writePuzzlePDF(puzzle)().toString).!!
//    }
  }
}