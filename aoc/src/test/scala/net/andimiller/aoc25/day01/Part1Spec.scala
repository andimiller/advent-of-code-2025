package net.andimiller.aoc25
package day01

import cats.data.NonEmptyList
import cats.effect.IO
import munit.CatsEffectSuite

class Part1Spec extends CatsEffectSuite:

  test("parse a file") {
    ReadResource[IO]
      .readWith("/day01example.txt")(Part1.fileParser)
      .assertEquals(
        NonEmptyList.of(
          Turn.Left(68),
          Turn.Left(30),
          Turn.Right(48),
          Turn.Left(5),
          Turn.Right(60),
          Turn.Left(55),
          Turn.Left(1),
          Turn.Left(99),
          Turn.Right(14),
          Turn.Left(82)
        )
      )
  }

  test("run the example file") {
    ReadResource[IO]
      .readWith("/day01example.txt")(Part1.fileParser)
      .map(steps => Part1.getSteps(steps.toList))
      .map(_.map(_.toInt))
      .assertEquals(
        List(
          50, 82, 52, 0, 95, 55, 0, 99, 0, 14, 32
        )
      )

  }

  test("parse part 1 input") {
    ReadResource[IO]
      .readWith("/day01-part1.txt")(Part1.fileParser)
      .map(_.size)
      .assertEquals(4664)
  }
