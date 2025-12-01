package net.andimiller.aoc25
package day01

import cats.data.NonEmptyList
import cats.effect.IO
import munit.CatsEffectSuite

class Part2Spec extends CatsEffectSuite:

  test("run the example file") {
    ReadResource[IO]
      .readWith("/day01example.txt")(Part1.fileParser)
      .map(steps => Part2.countTicks(steps.toList))
      .assertEquals(
        6
      )
  }

  test("do a big rotation") {
    assertEquals(
      obtained = Part2.countTicks(Turn.Right(1000).expand),
      expected = 10
    )
  }

  test("small movements") {
    assertEquals(
      obtained = Part2.countTicks(
        List(
          Turn.Left(50),
          Turn.Right(5),
          Turn.Left(5)
        )
      ),
      expected = 2
    )
  }
