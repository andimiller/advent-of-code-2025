package net.andimiller.aoc25
package day08

import cats.effect.IO
import cats.implicits.*
import munit.CatsEffectSuite
import net.andimiller.aoc25.day08.Part1.{calculateAnswer, link}

class Part1Spec extends CatsEffectSuite {
  test("example") {
    ReadResource[IO]
      .readWith("./day08-example.txt")(Point.parserMany)
      .map { points =>
        new GridPointIndex(points.toList.toVector)
      }
      .map(link(_)(10))
      .map(calculateAnswer)
      .assertEquals(40)
  }
}
