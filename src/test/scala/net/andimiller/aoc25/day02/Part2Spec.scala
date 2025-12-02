package net.andimiller.aoc25
package day02

import cats.effect.IO
import munit.CatsEffectSuite

class Part2Spec extends CatsEffectSuite:

  test("few quick tests") {
    assertEquals(
      obtained = Part2.isInvalid(99),
      expected = true
    )

    assertEquals(
      obtained = Part2.isInvalid(999),
      expected = true
    )
  }

  test("check the example again") {
    ReadResource[IO]
      .readWith("/day02-example.txt")(Ranges.parser)
      .map(rs => Part2.processRanges(rs))
      .assertEquals(
        4174379265L
      )
  }
