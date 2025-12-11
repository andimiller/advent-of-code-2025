package net.andimiller.aoc25
package day10

import cats.{Eval, Monad, Parallel}
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.std.{Console, Random}
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*
import spire.math.Rational


object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def minPresses(m: Machine): Int = {
    val buttonJoltages = m.buttons.map(Joltage.fromBitSet).toVector
    val numButtons = buttonJoltages.length
    val goalSize = m.joltage.toVector.size

    val matrix = (0 until goalSize).map { pos =>
      (0 until numButtons).map { btn =>
        Rational(buttonJoltages(btn).toVector.lift(pos).getOrElse(0))
      }.toVector
    }.toVector

    val b = m.joltage.toVector.map(Rational(_)).toVector

    GaussianElimination.minNonNegativeIntegerSolution(matrix, b).fold(0)(_.sum)
  }
  
  def program[F[_]: {Async, Parallel, Console, ReadResource, Clock, Bench, Random}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day10-input.txt")(Machine.machines)
        .bench("parse")
        .flatMap { machines =>
          machines.zipWithIndex.parTraverse { case (m, idx) =>
            Console[F].println(s"starting $idx") *> blocking {
              minPresses(m)
            } <* Console[F].println(s"finished $idx")
          }
        }
        .map(_.toList.sum)
        .flatTap(Console[F].println(_))
        .void
