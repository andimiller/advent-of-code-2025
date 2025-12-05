package net.andimiller.aoc25
package day05

import cats.effect.IO
import cats.implicits.*
import munit.CatsEffectSuite
import net.andimiller.aoc25.ReadResource

class Part1Spec extends CatsEffectSuite:

  test("example should work right") {
    ReadResource[IO]
      .readWith("day05-example.txt")(Inventory.parser)
      .map(Part1.getFreshIds)
      .assertEquals(
        Set(5L, 11L, 17L)
      )
  }
