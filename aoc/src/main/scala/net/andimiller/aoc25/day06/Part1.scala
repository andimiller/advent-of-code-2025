package net.andimiller.aoc25
package day06

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import Bench.syntax.*
import cats.data.NonEmptyList

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def calculateAnswers(w: Worksheet): NonEmptyList[Long] =
    w.problems.map:
      case Problem(numbers, op) =>
        op match
          case Operation.Add      =>
            numbers.reduce(_ + _)
          case Operation.Multiply =>
            numbers.reduce(_ * _)

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day06-input.txt")(Worksheet.parser)
        .bench("parse")
        .flatMap: w =>
          Async[F].blocking { calculateAnswers(w) }.bench("calculate")
        .flatMap: as =>
          Async[F].blocking { as.reduce }.bench("sum")
        .flatTap(Console[F].println(_))
        .void
