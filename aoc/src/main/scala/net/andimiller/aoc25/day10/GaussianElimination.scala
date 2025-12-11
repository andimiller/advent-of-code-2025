package net.andimiller.aoc25.day10


import spire.math.Rational
import spire.implicits._

// mostly done by claude, I just wanted to see that it was even possible
object GaussianElimination {

  type Matrix = Vector[Vector[Rational]]
  type Vec = Vector[Rational]

  def solve(a: Matrix, b: Vec): Option[(Vec, Set[Int])] = {
    val rows = a.length
    val cols = a.headOption.map(_.length).getOrElse(0)

    var m = a.zip(b).map { case (row, bi) => row :+ bi }
    var pivotRow = 0
    var pivotCols = Vector.empty[Int]

    for (col <- 0 until cols if pivotRow < rows) {
      val pivotIdx = (pivotRow until rows).find(r => m(r)(col) != Rational.zero)

      pivotIdx.foreach { maxRow =>
        val temp = m(pivotRow)
        m = m.updated(pivotRow, m(maxRow)).updated(maxRow, temp)

        val pivot = m(pivotRow)(col)
        m = m.updated(pivotRow, m(pivotRow).map(_ / pivot))

        for (row <- 0 until rows if row != pivotRow) {
          val factor = m(row)(col)
          if (factor != Rational.zero) {
            m = m.updated(row, m(row).zip(m(pivotRow)).map {
              case (a, b) => a - factor * b
            })
          }
        }

        pivotCols = pivotCols :+ col
        pivotRow += 1
      }
    }

    val inconsistent = m.exists { row =>
      row.dropRight(1).forall(_ == Rational.zero) && row.last != Rational.zero
    }

    if (inconsistent) None
    else {
      val freeVars = (0 until cols).toSet -- pivotCols.toSet
      val solution = Vector.tabulate(cols) { col =>
        pivotCols.indexOf(col) match {
          case -1 => Rational.zero
          case pr => m(pr).last
        }
      }
      Some((solution, freeVars))
    }
  }

  def minNonNegativeIntegerSolution(
                                     a: Matrix,
                                     b: Vec
                                   ): Option[Vector[Int]] = {
    solve(a, b).flatMap { case (baseSolution, freeVars) =>
      if (freeVars.isEmpty) {
        if (baseSolution.forall(r => r.isWhole && r >= Rational.zero))
          Some(baseSolution.map(_.toInt))
        else
          None
      } else {
        searchFreeVariables(a, b, baseSolution, freeVars.toVector)
      }
    }
  }

  private def searchFreeVariables(
                                   a: Matrix,
                                   b: Vec,
                                   baseSolution: Vec,
                                   freeVars: Vector[Int]
                                 ): Option[Vector[Int]] = {
    val cols = a.head.length

    // Compute null space basis vectors
    val nullBasis: Vector[Vec] = freeVars.map { freeVar =>
      solve(a, Vector.fill(a.length)(Rational.zero)).map { case (_, _) =>
        // For each free var, set it to 1 and solve
        val modifiedA = a
        val modifiedB = a.map(row => -row(freeVar))
        solve(
          a.map(row => row.zipWithIndex.filterNot(_._2 == freeVar).map(_._1)),
          modifiedB
        ) match {
          case Some((sol, _)) =>
            var vec = Vector.fill(cols)(Rational.zero)
            vec = vec.updated(freeVar, Rational.one)
            var idx = 0
            for (c <- 0 until cols if c != freeVar) {
              vec = vec.updated(c, sol(idx))
              idx += 1
            }
            vec
          case None =>
            Vector.fill(cols)(Rational.zero).updated(freeVar, Rational.one)
        }
      }.getOrElse(Vector.fill(cols)(Rational.zero).updated(freeVar, Rational.one))
    }

    // Search range based on goal values
    val maxSearch = b.map(r => Math.abs(r.toInt)).max + 1
    val searchRange = -maxSearch to maxSearch

    def enumerate(idx: Int, current: Vec): Iterator[Vector[Int]] = {
      if (idx >= nullBasis.length) {
        if (current.forall(r => r.isWhole && r >= Rational.zero))
          Iterator(current.map(_.toInt))
        else
          Iterator.empty
      } else {
        searchRange.iterator.flatMap { t =>
          val next = current.zip(nullBasis(idx)).map { case (c, n) => c + Rational(t) * n }
          enumerate(idx + 1, next)
        }
      }
    }

    enumerate(0, baseSolution).minByOption(_.sum)
  }
}