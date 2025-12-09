package net.andimiller.aoc25
package day09

import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import Math.abs

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def rectangleSize(t1: Tile, t2: Tile): Long =
    (abs(t1.x - t2.x) + 1) * (abs(t1.y - t2.y) + 1)

  def findLargestRectangle(tiles: NonEmptyList[Tile]): Long =
    tiles.toList
      .combinations(2)
      .map { case a :: b :: Nil =>
        rectangleSize(a, b)
      }
      .max

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day09-input.txt")(Tile.tiles)
        .bench("parse")
        .flatMap { tiles =>
          blocking { findLargestRectangle(tiles) }.bench("findLargestRectangle")
        }
        .flatTap(Console[F].println(_))
        .void
