package net.andimiller.aoc25.day07

import munit.CatsEffectSuite
import cats.implicits.*

class Part2Spec extends CatsEffectSuite {
  test("semigroup") {

    assertEquals(
      obtained = Vector(
        Map(1 -> 1, 2 -> 1),
        Map(1 -> 1)
      ).combineAll,
      expected = Map(1 -> 2, 2 -> 1)
    )

  }
}
