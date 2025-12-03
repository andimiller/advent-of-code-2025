package net.andimiller.aoc25
package day03

import cats.data.NonEmptyList
import cats.effect.IO
import munit.CatsEffectSuite

class Part1Spec extends CatsEffectSuite:

  test("process the example data") {
    ReadResource[IO]
      .readWith("/day03-example.txt")(BatteryBank.banks)
      .map(rs => rs.map(Part1.findMaxJoltage))
      .assertEquals(
        NonEmptyList.of(
          98,
          89,
          78,
          92
        )
      )
  }
