package net.andimiller.aoc25
package day05

import cats.Monad
import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.kernel.Order
import cats.mtl.Tell
import cats.syntax.all.{catsSyntaxPartialOrder as _, *}
import spire.compat.{integral, ordering}
import spire.implicits.{LongAlgebra, LongTag}
import spire.math.Interval
import spire.math.interval.ValueBound
import fs2.Stream
import Bench.syntax.*

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  extension (i: Interval[Long])
    def size: Long = {
      val ValueBound(lower) = i.lowerBound
      val ValueBound(upper) = i.upperBound
      if (lower == upper)
        0L
      else
        upper - lower + 1
    }

  given Order[Interval[Long]] =
    Order.by { i =>
      val ValueBound(lower) = i.lowerBound
      val ValueBound(upper) = i.upperBound
      lower -> upper
    }

  @tailrec
  def tryMerge(a: Interval[Long], b: Interval[Long]): Option[Interval[Long]] =
    if (a > b)
      tryMerge(b, a)
    else if ((a intersects b) || (a.upperBound + 1) == b.lowerBound)
      Some(a union b)
    else
      None

  def mergeRanges(i: Inventory): List[Interval[Long]] =
    i.fresh.sorted.foldLeft(List.empty[Interval[Long]]) {
      case (Nil, i)          => List(i)
      case (head :: tail, i) =>
        tryMerge(head, i) match
          case Some(merged) => merged :: tail
          case None         => i :: head :: tail
    }

  def count(intervals: List[Interval[Long]]): Long =
    intervals.iterator.map(_.size).sum

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    Bench[F].bencher.use { implicit bencher =>
      ReadResource[F]
        .readWith("./day05-input.txt")(Inventory.parser)
        .bench("parse")
        .flatMap(i => Async[F].blocking { mergeRanges(i) }.bench("merge"))
        .flatMap(r => Async[F].blocking { count(r) }.bench("count"))
        .flatTap(Console[F].println(_))
        .void
    }
