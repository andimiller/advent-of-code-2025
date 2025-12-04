package net.andimiller.aoc25
package day04

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def transformGrid(g: Grid[Tile]): Grid[Tile] =
    g.zipWithCoordinates.map {
      case ((x, y), Tile.Paper) =>
        g.neighbours(x, y).count(_ == Tile.Paper) match
          case i if i < 4 => Tile.ToBeRemoved
          case _          => Tile.Paper
      case (_, other)           => other
    }

  def countAccessibleTiles(g: Grid[Tile]): Int =
    transformGrid(g).iterator_.count(_ == Tile.ToBeRemoved)

  def program[F[_]: {Async, Console, ReadResource, Clock}] =
    ReadResource[F]
      .readWith("./day04-input.txt")(Grid.parser)
      .map(countAccessibleTiles)
      .flatTap(Console[F].println(_))
      .void
