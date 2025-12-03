package net.andimiller.aoc25
package day02

import cats.effect.IO
import munit.CatsEffectSuite

class Part1Spec extends CatsEffectSuite:

  test("process the example data") {
    ReadResource[IO]
      .readWith("/day02-example.txt")(Ranges.parser)
      .map(rs => Part1.processRanges(rs))
      .assertEquals(
        1227775554L
      )
  }
