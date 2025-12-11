package net.andimiller.aoc25
package day10

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.*
import munit.CatsEffectSuite
import net.andimiller.aoc25.day08.Part1.{calculateAnswer, link}

class Part1Spec extends CatsEffectSuite {

  test("example") {
    ReadResource[IO]
      .readWith("./day10-example.txt")(Machine.machines)
      .map { machines =>
        machines.map(Part1.solveRaw(_))
      }
      .assertEquals(
        NonEmptyList.of(
          Set(4, 5),
          Set(2, 3, 4),
          Set(1, 2)
        )
      )
  }

}
