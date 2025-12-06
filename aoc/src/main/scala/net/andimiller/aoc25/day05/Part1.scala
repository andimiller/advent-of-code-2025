package net.andimiller.aoc25
package day05

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def getFreshIds(i: Inventory): Set[Long] =
    i.available.filter { ingredient =>
      i.fresh.exists(_.contains(ingredient))
    }

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    Bench[F].bench(
      ReadResource[F]
        .readWith("./day05-input.txt")(Inventory.parser)
        .flatMap { i =>
          Bench[F].bench(
            Async[F].blocking {
              getFreshIds(i)
            }
          )
        }
        .map(_.size)
        .flatTap(Console[F].println(_))
        .void
    )
