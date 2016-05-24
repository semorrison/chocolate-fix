package net.tqft.fix

object CellState {
  var combineCounter = 0
}

sealed trait CellState {
  def combine(other: CellState): Option[CellState]
  def weaker: Iterator[CellState]
}
case object Empty extends CellState {
  override def toString = ""
  override def combine(other: CellState) = {
    Some(other)
  }
  override def weaker = Iterator.empty
}
case class Partial(characteristics: Seq[Option[Int]]) extends CellState {
  override def toString = characteristics.map({
    case None => "?"
    case Some(i) => i.toString
  }).mkString(",")
  override def combine(other: CellState) = {
    other match {
      case Empty => Some(this)
      case Partial(otherCharacteristics) => {
        CellState.combineCounter = CellState.combineCounter + 1
        val zipped = characteristics.zip(otherCharacteristics).map({
          case (None, None) => Some(None)
          case (None, Some(i)) => Some(Some(i))
          case (Some(i), None) => Some(Some(i))
          case (Some(i), Some(j)) => {
            if (i == j) Some(Some(i)) else None
          }
        })
        if (zipped.forall(_.nonEmpty)) {
          if (zipped.forall(_.get.nonEmpty)) {
            Some(Filled(zipped.map(_.get.get)))
          } else {
            Some(Partial(zipped.map(_.get)))
          }
        } else {
          None
        }
      }
      case f: Filled => f.combine(this)
    }
  }
  override def weaker: Iterator[CellState] = {
    if (characteristics.count(_.nonEmpty) == 1) {
      Iterator(Empty)
    } else {
      for (i <- (0 until characteristics.size).iterator; if characteristics(i).nonEmpty) yield Partial(characteristics.updated(i, None))
    }
  }
}

case class Filled(characteristics: Seq[Int]) extends CellState {
  override def toString = characteristics.mkString(",")
  override def combine(other: CellState) = {
    other match {
      case Empty => Some(this)
      case Partial(otherCharacteristics) => {
        CellState.combineCounter = CellState.combineCounter + 1
        if (characteristics.zip(otherCharacteristics).forall({
          case (i, None) => true
          case (i, Some(j)) => i == j
        })) Some(this) else None
      }
      case f: Filled => {
        CellState.combineCounter = CellState.combineCounter + 1
        if (this == f) Some(this) else None
      }
    }
  }
  override def weaker = {
    for (i <- (0 until characteristics.size).iterator) yield Partial(characteristics.map(Some(_)).updated(i, None))
  }
}

object Board {
  def empty(dimensions: Seq[Int]) = Board(dimensions, IndexedSeq.fill(dimensions.reduce(_ * _))(Empty))
}
case class Board(dimensions: Seq[Int], states: IndexedSeq[CellState]) {
  override def toString: String = {
    require(dimensions.size == 2)
    val filler = Seq.fill(dimensions(1))("-----").mkString("+", "+", "+\n")
    filler + states.grouped(dimensions(1)).map({ row =>
      row.map(c => (c.toString + "   ").take(3)).mkString("| ", " | ", " |\n") + filler
    }).mkString("")
  }

  lazy val weight = states.map({
    case Empty => 1
    case p: Partial => 2
    case f: Filled => 3
  }).sum

  lazy val filledStates = coordinates().toSeq.map(Filled(_))
  lazy val remainingPieces = filledStates.toSet -- states.collect({ case s: Filled => s })
  lazy val partialProducts = dimensions.reverse.scanLeft(1)(_ * _).reverse.tail
  def coordinates(dimensions: Seq[Int] = dimensions) = dimensions.foldLeft[Iterator[IndexedSeq[Int]]](Iterator(IndexedSeq.empty))({
    case (iterator, n) => iterator.flatMap(c => (0 until n).map(i => c :+ i))
  })
  def index(location: IndexedSeq[Int]): Int = {
    location.zip(partialProducts).map(p => p._1 * p._2).sum
  }
  def consistent: Option[Board] = {
    val filled = states.filter(_.isInstanceOf[Filled])
    if (filled.size == filled.distinct.size) {
      // check that there aren't too many squares, etc.
      if ((for (r <- (0 until dimensions.size).iterator; i <- 0 until dimensions(r)) yield states.count({
        case Empty => false
        case Partial(characteristics) => characteristics(r) == Some(i)
        case Filled(characteristics) => characteristics(r) == i
      }) <= (dimensions.product / dimensions(r))).forall(_ == true)) {
        Some(this)
      } else {
        None
      }
    } else {
      None
    }
  }
  def combine(puzzlePiece: Board): Iterator[Board] = {
    val offsets = coordinates(dimensions.zip(puzzlePiece.dimensions).map(p => p._1 + 1 - p._2))
    offsets.flatMap(offset => combine(puzzlePiece, offset))
  }
  def combine(puzzlePiece: Board, offset: IndexedSeq[Int]): Option[Board] = {
    val results = puzzlePiece.coordinates().foldLeft[Option[Board]](Some(this))({
      case (None, _) => None
      case (Some(board), c) => {
        val cOffset = c.zip(offset).map(p => p._1 + p._2)
        board.states(index(cOffset)).combine(puzzlePiece.states(puzzlePiece.index(c))) match {
          case None => None
          case Some(newCellState) => Some(board.copy(states = board.states.updated(index(cOffset), newCellState)))
        }
      }
    })
    results.flatMap(_.consistent)
  }
  def solve: Board = {
    require(dimensions.size == 2)
    val targets = for (
      r <- 0 until dimensions.size;
      i <- 0 until dimensions(r);
      ri = remainingPieces.filter(f => f.characteristics(r) == i);
      if ri.size == 1;
      p = ri.head;
      (s: Partial, k) <- states.zipWithIndex;
      if s.characteristics(r) == i
    ) yield (r, p, k)
    if (targets.nonEmpty) {
      targets.foldLeft(this)({ case (b, (r, p, k)) => b.copy(states = states.updated(k, p)) }).solve
    } else {
      this
    }
  }
  def placeNextPiece: Iterator[Board] = {
    val nextPiece = remainingPieces.head
    combine(Board(Seq.fill(dimensions.size)(1), IndexedSeq(nextPiece)))
  }
  def placeRemainingPieces: Iterator[Board] = {
    if (remainingPieces.isEmpty) {
      Iterator(this)
    } else {
      placeNextPiece.flatMap(_.placeRemainingPieces)
    }
  }
  def weakerClues: Iterator[Board] = {
    for (i <- (0 until states.size).iterator; c <- states(i).weaker) yield copy(states = states.updated(i, c))
  }
}

case class Puzzle(board: Board, puzzlePieces: Seq[Board]) {
  override def toString = puzzlePieces.mkString("\n\n")
  def combinePiece(solving: Boolean): Iterator[Puzzle] = {
    val combined = board.combine(puzzlePieces.head)
    val solved = if (solving) combined.map(_.solve) else combined
    solved.map(Puzzle(_, puzzlePieces.tail))
  }
  def combinePieces(solving: Boolean): Iterator[Puzzle] = {
    if (puzzlePieces.isEmpty) {
      Iterator(if (solving) copy(board = board.solve) else this)
    } else {
      combinePiece(solving).flatMap(_.combinePieces(solving))
    }
  }
  def solutions = combinePieces(true).flatMap(_.board.placeRemainingPieces)
  lazy val twoSolutions = {
    val sols = solutions
    if (sols.hasNext) {
      val s1 = sols.next
      var s2 = s1
      while (sols.hasNext && s2 == s1) {
        s2 = sols.next
      }
      if (s2 == s1) {
        Seq(s1)
      } else {
        Seq(s1, s2)
      }
    } else {
      Seq.empty
    }
  }

  def difficulty = {
    CellState.combineCounter = 0
    solutions.size
    CellState.combineCounter
  }
  //  def difficulty = puzzlePieces.permutations.map(p => Puzzle(board, p).fixedOrderDifficulty).min
  lazy val unique_? = {
    twoSolutions.size == 1
  }
  lazy val minimal_? = {
    unique_? && puzzlePieces.combinations(puzzlePieces.size - 1).forall(pieces => !Puzzle(board, pieces).unique_?)
  }
  def weakerClues: Iterator[Puzzle] = {
    for (i <- (0 until puzzlePieces.size).iterator; b <- puzzlePieces(i).weakerClues) yield Puzzle(board, puzzlePieces.updated(i, b))
  }
  lazy val clueMinimal_? = {
    unique_? && !weakerClues.exists(_.unique_?)
  }
  def clueMinimize: Puzzle = {
    weakerClues.find(_.unique_?).map(_.clueMinimize).getOrElse(this)
  }
  def minimize: Puzzle = {
    puzzlePieces.combinations(puzzlePieces.size - 1).map(pieces => Puzzle(board, pieces)).find(_.unique_?).map(_.minimize).getOrElse(this)
  }
  def addPuzzlePiece: Puzzle = {
    // we first find two solutions
    val Seq(s1, s2) = twoSolutions
    // then find somewhere they differ
    val Some(location) = board.coordinates().find(c => s1.states(board.index(c)) != s2.states(board.index(c)))
    // decide the piece size 
    def random(n: Int, p: Double) = Seq.fill(n)(if (scala.util.Random.nextDouble > p) 0 else 1).sum

    val pieceDimensions = {
      var result = IndexedSeq.fill(board.dimensions.size)(0)
      while (result.product <= 1) {
        result = IndexedSeq.tabulate(board.dimensions.size)(i => random(board.dimensions(i), 0.5))
      }
      result
    }
    val emptyPiece = Board.empty(pieceDimensions)
    // decide the offset
    val pieceOffset = {
      def randomOffset = IndexedSeq.tabulate(board.dimensions.size)(i => scala.util.Random.nextInt(board.dimensions(i) + 1 - pieceDimensions(i)))
      var result = randomOffset
      while (!emptyPiece.coordinates().map(c => result.zip(c).map(p => p._1 + p._2)).contains(location)) {
        result = randomOffset
      }
      result
    }
    // decide which squares to lift
    val newPiece = Board(pieceDimensions, emptyPiece.coordinates().toIndexedSeq.map({
      c =>
        {
          val cOffset = pieceOffset.zip(c).map(p => p._1 + p._2)
          val state = s1.states(s1.index(cOffset)).asInstanceOf[Filled]
          val p = if (cOffset == location) { 1.0 } else scala.math.random
          if (p < 0.6) {
            Empty
          } else if (p < 0.8) {
            val q = scala.math.random
            if (q < 0.5) {
              Partial(Seq(None, Some(state.characteristics(1))))
            } else {
              Partial(Seq(Some(state.characteristics(0)), None))
            }
          } else {
            state
          }
        }
    }))
    Puzzle(board, (puzzlePieces :+ newPiece).sortBy(-_.weight))
  }
  def addPuzzlePiecesUntilUnique: Puzzle = {
    if (unique_?) {
      this
    } else {
      println(puzzlePieces.size)
      //      if(puzzlePieces.size > 10) {
      //      import scala.sys.process._
      //      ("open " + RenderBoard.writePuzzlePDF(this)().toString).!!
      //      }
      addPuzzlePiece.addPuzzlePiecesUntilUnique
    }
  }
  def shuffle = copy(puzzlePieces = scala.util.Random.shuffle(puzzlePieces))
  def amalgamate: Puzzle = {
    val target = for (
      f <- board.filledStates.toStream;
      pieces = puzzlePieces.filter(_.states.contains(f));
      if pieces.size > 1
    ) yield (f, pieces(0), pieces(1))
    println("amalgamate targets: " + target.size)
    (target).headOption match {
      case None => this
      case Some((filled, p0, p1)) => {
        val c0 = p0.coordinates().find(c => p0.states(p0.index(c)) == filled).get
        val c1 = p1.coordinates().find(c => p1.states(p1.index(c)) == filled).get
        val mins = c0.zip(c1).map({ case (x0, x1) => -Seq(x0, x1).max })
        val maxs = c0.zip(c1).zip(p0.dimensions).zip(p1.dimensions).map({ case (((x0, x1), n0), n1) => Seq(n0 - x0, n1 - x1).max })
        val empty = Board.empty(maxs.zip(mins).map({ p => p._1 - p._2 }))
        val offset0: IndexedSeq[Int] = c0.zip(c1).map({ case (x0, x1) => Seq(x1 - x0, 0).max })
        val offset1: IndexedSeq[Int] = c0.zip(c1).map({ case (x0, x1) => Seq(x0 - x1, 0).max })
        val combined0 = p0.coordinates().foldLeft(empty)({
          case (b, c) => b.copy(states = b.states.updated(b.index(offset0.zip(c).map(p => p._1 + p._2)), p0.states(p0.index(c))))
        })
        val combined1 = p1.coordinates().foldLeft(combined0)({
          case (b, c) => b.copy(states = b.states.updated(b.index(offset1.zip(c).map(p => p._1 + p._2)), p1.states(p1.index(c))))
        })

        copy(puzzlePieces = (((puzzlePieces.toSet - p0) - p1) + combined1).toSeq).amalgamate
      }
    }
  }
}