package net.andimiller.aoc25
package day07

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import Bench.syntax.*
import cats.Endo
import cats.data.Writer

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def cascade(b: Board): (Int, Set[Int]) =
    b.rows.foldLeft(0 -> Set.empty[Int]) { case ((counter, beams), cells) =>
      val indexedCells                   = cells.zipWithIndex
      val changes: Endo[(Int, Set[Int])] = indexedCells
        .collect[Endo[(Int, Set[Int])]] {
          case (Cell.Start, idx)                  => { case (splits, bms) => splits -> (bms + idx) }
          case (Cell.Splitter, idx) if beams(idx) => { case (splits, bms) => (splits + 1) -> (bms - idx + (idx + 1) + (idx - 1)) }
        }
        .reduceOption(_ andThen _)
        .getOrElse(identity)
      changes((counter, beams))
    }

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day07-input.txt")(Board.parser)
        .bench("parse")
        .flatMap: b =>
          Async[F].blocking { cascade(b) }.bench("cascade")
        .flatTap(Console[F].println(_))
        .void
