package net.andimiller.aoc25
package day02

import cats.effect.std.Console
import cats.effect.{IO, IOApp, Sync}
import cats.implicits.*
import fs2.Stream

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def isInvalid(i: Long): Boolean = {
    val s = i.toString
    LazyList
      .range(2, s.length + 1)
      .filter(i => s.length % i == 0) // if we can evenly divide it up by this
      .map { i =>
        s.grouped(s.length / i).distinct.size == 1
      }
      .exists(identity)
  }

  def processRanges(r: Ranges): Long =
    Stream
      .emits(r.contents.toList)
      .flatMap { case (start, end) =>
        Stream.range(start, end + 1)
      }
      .filter(isInvalid)
      .compile
      .foldMonoid

  def program[F[_]: {Sync, Console, ReadResource}] =
    ReadResource[F]
      .readWith("./day02-input.txt")(Ranges.parser)
      .flatMap { ranges =>
        val total = processRanges(ranges)
        Console[F].println(s"total of invalid numbers was $total")
      }
      .void
