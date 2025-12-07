package net.andimiller.aoc25
package day07

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import Bench.syntax.*

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def cascade(b: Board): Map[Int, Long] =
    b.rows.foldLeft(
      Map.empty[Int, Long]
    ) { case (timelineDb, cells) =>
      val indexedCells                              = cells.zipWithIndex
      val newTimelines: Map[Int, Long]              = indexedCells.collect { case (Cell.Start, idx) =>
        Map(idx -> 1L)
      }.combineAll
      val advancedTimelines: Vector[Map[Int, Long]] = timelineDb.toVector.map { case (state, count) =>
        val splits = indexedCells.collect {
          case (Cell.Splitter, idx) if state == idx =>
            Vector(
              Map((idx - 1) -> count),
              Map((idx + 1) -> count)
            )
        }
        if (splits.isEmpty)
          Map(state -> count)
        else splits.flatten.combineAll
      }
      newTimelines |+| advancedTimelines.combineAll
    }

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day07-input.txt")(Board.parser)
        .flatMap: b =>
          Async[F].blocking { cascade(b) }.bench("cascade")
        .map: timelines =>
          timelines.values.sum
        .flatTap(Console[F].println(_))
        .void
