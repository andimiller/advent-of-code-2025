package net.andimiller.aoc25

import cats.{Monad, Show}
import cats.implicits.*
import cats.effect.{Async, Ref, Resource}
import cats.effect.kernel.Clock
import cats.effect.std.Console
import cats.kernel.Order
import sourcecode.{Args, FileName, Line, Name}
import fs2.Stream

import scala.collection.immutable.ListMap
import scala.concurrent.duration.*

trait Bencher[F[_]]:
  def bench[T](fa: F[T], iterations: Int = 100, name: String = "")(using file: sourcecode.FileName, line: sourcecode.Line): F[T]
  def report: F[Unit] // reports right now, you don't need to call this if you're using the resource, it'll report on close

trait Bench[F[_]]:
  def bench[T](fa: F[T], iterations: Int = 100)(using name: sourcecode.Name): F[T]
  def bencher: Resource[F, Bencher[F]]

object Bench:
  def apply[F[_]](using b: Bench[F]): Bench[F] = b

  private val units = List(
    DAYS         -> "d",
    HOURS        -> "h",
    MINUTES      -> "m",
    SECONDS      -> "s",
    MILLISECONDS -> "ms",
    MICROSECONDS -> "µs",
    NANOSECONDS  -> "ns"
  )

  given Show[FiniteDuration] = Show.show { d =>
    val (_, parts) = units.foldLeft((d.toNanos.abs, Vector.empty[String])):
      case ((remaining, acc), (unit, suffix)) =>
        val divisor = unit.toNanos(1)
        val value   = remaining / divisor
        (remaining % divisor, if value > 0 then acc :+ s"$value$suffix" else acc)
    if parts.isEmpty then "0ns" else parts.mkString(" ")
  }

  given Order[FiniteDuration] = cats.implicits.catsKernelStdOrderForFiniteDuration

  given fromAsync[F[_]: {Async, Console, Clock}]: Bench[F] = new Bench[F]:
    // Util for padding the grid to print it nicely
    private def padGrid(grid: Vector[Vector[String]], padChar: Char = ' '): Vector[Vector[String]] =
      assume(grid.isEmpty || grid.forall(_.length == grid.head.length), "all rows must have the same width")
      val widths = grid.flatMap(_.zipWithIndex).groupMapReduce(_._2)(_(0).length)(_ max _)
      grid.map: row =>
        row.zipWithIndex.map: (cell, idx) =>
          cell.padTo(widths.getOrElse(idx, 0), padChar)

    // Universal report method to report a database of benchmark results
    private def reportDb(db: Vector[(String, Vector[FiniteDuration])]): F[Unit] = {
      val maxNameWidth: Int = db.map(_._1).map(_.length).maxOption.getOrElse(0)

      val headers = Vector("benchmark", "avg", "min", "max", "iters")
      val body    = db.map { case (name, times) =>
        val total = times.reduce(_ plus _)
        Vector(name.show, (total / times.size).show, times.min.show, times.max.show, times.size.show)
      }

      padGrid(body.prepended(headers)).traverse_ { g =>
        Console[F].println(g.mkString(" | "))
      }
    }

    override def bench[T](fa: F[T], iterations: Int = 100)(using name: Name): F[T] =
      Stream.eval(Clock[F].timed(fa)).repeatN(iterations).compile.toVector.flatMap { results =>
        reportDb(
          Vector(name.value -> results.map(_._1))
        ).as(results.head._2)
      }

    override def bencher: Resource[F, Bencher[F]] =
      Resource
        .make(Ref.of[F, Vector[(String, Vector[FiniteDuration])]](Vector.empty)) { ref =>
          ref.get.flatTap(reportDb).void
        }
        .map { ref =>
          new Bencher[F]:
            override def bench[T](fa: F[T], iterations: Int = 100, name: String = "")(using file: FileName, line: Line): F[T] =
              val n = if (name.nonEmpty) name else s"${file.value}:${line.value}"

              Stream.eval(Clock[F].timed(fa)).repeatN(iterations).compile.toVector.flatMap { results =>
                ref
                  .update { db =>
                    db.appended(n -> results.map(_._1))
                  }
                  .as(results.head._2)
              }

            override def report: F[Unit] = ref.get.flatTap(reportDb).void
        }

  object syntax:
    extension [F[_]: {Bencher, Monad}, T](f: F[T])
      def bench(name: String = "", iterations: Int = 100)(using file: FileName, line: Line): F[T] =
        implicitly[Bencher[F]].bench(f, iterations, name)
