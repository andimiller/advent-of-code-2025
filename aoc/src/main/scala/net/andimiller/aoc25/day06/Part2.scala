package net.andimiller.aoc25
package day06

import cats.data.NonEmptyList
import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import Bench.syntax.*

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def calculateAnswers(w: Worksheet): NonEmptyList[Long] =
    w.problems.map:
      case Problem(numbers, op) =>
        op match
          case Operation.Add      =>
            numbers.reduce(using _ + _)
          case Operation.Multiply =>
            numbers.reduce(using _ * _)

  def transpose(s: String): String =
    s.linesIterator.toVector.transpose.map(_.mkString).mkString(System.lineSeparator()) + System.lineSeparator()

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWithPretransform("day06-input.txt")(transpose)(Worksheet2.parser)
        .map(calculateAnswers)
        .map(_.sumAll)
        .flatTap(Console[F].println(_))
        .void
