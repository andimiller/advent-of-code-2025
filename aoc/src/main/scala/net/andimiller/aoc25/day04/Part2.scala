package net.andimiller.aoc25
package day04

import cats.{Eval, Monad}
import cats.data.{Writer, WriterT}
import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def removeAccessiblePaper(g: Grid[Tile]): Writer[Int, Grid[Tile]] =
    Part1
      .transformGrid(g)
      .traverse:
        case Tile.ToBeRemoved =>
          Writer.tell(1).as(Tile.Empty)
        case t                =>
          Writer.value[Int, Tile](t)

  def loopTilMaxRemoved(g: Grid[Tile]): WriterT[Eval, Int, Grid[Tile]] =
    Monad[[X] =>> WriterT[Eval, Int, X]].tailRecM(g) { gi =>
      val (count, next) = removeAccessiblePaper(gi).run
      if (count == 0)
        WriterT(Eval.now(count, Right(next)))
      else
        WriterT(Eval.now(count, Left(next)))
    }

  def program[F[_]: {Async, Console, ReadResource, Clock}] =
    ReadResource[F]
      .readWith("./day04-input.txt")(Grid.parser)
      .flatMap { g =>
        val (removed, finalGrid) = loopTilMaxRemoved(g).run.value
        Console[F].println(s"$removed papers removed") *> Console[F].println(finalGrid)
      }
      .void
