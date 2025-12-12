package net.andimiller.aoc25
package day11

import cats.{Monad, Semigroup}
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.std.{Console, Random}
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import scala.collection.BitSet

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  case class Counters(started: Long, dac: Long, fft: Long, dacAndFft: Long)
  object Counters:
    val empty = Counters(0, 0, 0, 0)
    given Semigroup[Counters] with
      override def combine(a: Counters, b: Counters): Counters =
        Counters(
          started = a.started + b.started,
          dac = a.dac + b.dac,
          fft = a.fft + b.fft,
          dacAndFft = a.dacAndFft + b.dacAndFft
        )

  def countRoutes(r: Routes): Map[Label, Counters] = {
    def go(activeRoutes: Map[Label, Counters]): Map[Label, Counters] = {

      val next = activeRoutes.iterator
        .map { case (label, counters) =>
          r.routes
            .get(label)
            .fold(List.empty)(_.toList)
            .iterator
            .map {
              case "dac"       =>
                // move everything from started => dac, and everything from fft => dacAndFft
                Map(
                  "dac" -> Counters(
                    started = 0,
                    dac = counters.started,
                    fft = 0,
                    dacAndFft = counters.fft
                  )
                )
              case "fft"       =>
                // move everything from started => fft, and everything from dac => dacAndFft
                Map(
                  "fft" -> Counters(
                    started = 0,
                    dac = 0,
                    fft = counters.started,
                    dacAndFft = counters.dac
                  )
                )
              case destination =>
                Map(destination -> counters)
            }
            .foldLeft(Map.empty[Label, Counters])(_ |+| _)
        }
        .fold(Map(Label.end -> activeRoutes.getOrElse(Label.end, Counters.empty)))(_ |+| _)

      if (next.keySet != Set(Label.end))
        go(next)
      else
        next
    }

    go(
      activeRoutes = Map("svr" -> Counters.empty.copy(started = 1))
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
