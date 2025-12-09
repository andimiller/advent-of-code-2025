package net.andimiller.aoc25
package day09

import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import java.lang.Math.abs

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def rectangleSize(t1: Tile, t2: Tile): Long =
    (abs(t1.x - t2.x) + 1) * (abs(t1.y - t2.y) + 1)

  def findLargestRectangle(reds: NonEmptyList[Tile], tt: TileTracker): Long =
    reds.toList
      .combinations(2)
      .map { case a :: b :: Nil =>
        (a, b)
      }
      .filter(tt.rectangleIsValid)
      .map(rectangleSize)
      .max

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day09-input.txt")(Tile.tiles)
        .bench("parse")
        .flatMap { tiles =>
          blocking {
            new TileTracker(tiles)
          }.bench("tracker", iterations = 1)
        }
        .flatMap { tt =>
          blocking {
            findLargestRectangle(tt.redTiles, tt)
          }.bench("find", iterations = 1)
        }
        .flatTap(Console[F].println(_))
        .void
