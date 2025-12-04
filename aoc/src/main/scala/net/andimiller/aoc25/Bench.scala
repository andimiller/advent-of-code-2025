package net.andimiller.aoc25

import cats.Show
import cats.implicits.*
import cats.effect.Async
import cats.effect.kernel.Clock
import cats.effect.std.Console
import sourcecode.{Args, Name}
import fs2.Stream

import scala.concurrent.duration.{Duration, DurationLong}

trait Bench[F[_]]:
  def bench[T](fa: F[T], iterations: Int = 100)(using name: sourcecode.Name): F[T]

object Bench:
  def apply[F[_]](using b: Bench[F]): Bench[F] = b

  given Show[Duration] = Show.show { d =>
    val secs = d.toSeconds.seconds
    val ms   = d.minus(secs).toMillis.milliseconds
    val ns   = d.minus(secs).minus(ms).toNanos.nanoseconds
    s"${secs.toSeconds}s ${ms.toMillis}ms ${ns.toNanos}ns"
  }

  given fromAsync[F[_]: {Async, Console, Clock}]: Bench[F] = new Bench[F]:
    override def bench[T](fa: F[T], iterations: Int = 100)(using name: Name): F[T] =
      Stream.eval(Clock[F].timed(fa)).repeatN(iterations).compile.toVector.flatMap { results =>
        val times: Vector[Duration] = results.map(_._1)
        val total                   = times.reduce(_ plus _)
        Console[F]
          .println(show"Bench: ${name.toString} min: ${times.min}, max: ${times.max}, avg:${total / times.size}, iter:$iterations")
          .as(results.head._2)
      }
