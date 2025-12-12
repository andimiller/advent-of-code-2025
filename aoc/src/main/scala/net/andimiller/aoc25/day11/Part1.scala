package net.andimiller.aoc25
package day11

import cats.Monad
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.std.{Console, Random}
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import scala.collection.BitSet

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def countRoutes(r: Routes): Map[Label, Long] = {
    def go(activeRoutes: Map[Label, Long]): Map[Label, Long] = {
      val next = activeRoutes.iterator
        .map { case (label, count) =>
          r.routes
            .get(label)
            .fold(List.empty)(_.toList)
            .iterator
            .map { destination =>
              Map(destination -> count)
            }
            .foldLeft(Map.empty[Label, Long])(_ |+| _)
        }
        .fold(Map(Label.end -> activeRoutes.getOrElse(Label.end, 0L)))(_ |+| _)

      if (next.keySet != Set(Label.end))
        go(next)
      else
        next
    }

    go(
      activeRoutes = Map("svr" -> 1)
    )
  }

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench, Random}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day11-input.txt")(Routes.parser)
        .bench("parse")
        .map { routes =>
          countRoutes(routes)
        }
        .flatTap(Console[F].println(_))
        .void
