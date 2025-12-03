package net.andimiller.aoc25
package day03

import cats.data.NonEmptyList
import cats.effect.IO
import munit.CatsEffectSuite

class Part2Spec extends CatsEffectSuite:

  test("process the example data") {
    ReadResource[IO]
      .readWith("/day03-example.txt")(BatteryBank.banks)
      .map(rs => rs.map(Part2.findMaxJoltage(_)))
      .assertEquals(
        NonEmptyList.of(
          987654321111L,
          811111111119L,
          434234234278L,
          888911112111L
        )
      )
  }
