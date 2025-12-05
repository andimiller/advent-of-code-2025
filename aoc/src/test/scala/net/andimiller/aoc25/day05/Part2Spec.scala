package net.andimiller.aoc25
package day05

import cats.effect.IO
import cats.implicits.*
import munit.CatsEffectSuite
import net.andimiller.aoc25.ReadResource
import spire.math.Interval

class Part2Spec extends CatsEffectSuite:

  test("merging intervals should work right") {
    import Part2.tryMerge
    assertEquals(
      obtained = tryMerge(Interval.closed(1L, 5L), Interval.closed(6L, 10L)),
      expected = Some(Interval.closed(1L, 10L)),
      clue = "Merging two that are next to each other but share no common elements"
    )

    assertEquals(
      obtained = tryMerge(Interval.closed(6L, 10L), Interval.closed(1L, 5L)),
      expected = Some(Interval.closed(1L, 10L)),
      clue = "Merging two that are next to each other but share no elements, reversed"
    )

    assertEquals(
      obtained = tryMerge(Interval.closed(1L, 8L), Interval.closed(3L, 10L)),
      expected = Some(Interval.closed(1L, 10L)),
      clue = "Merging two that overlap"
    )

    assertEquals(
      obtained = tryMerge(Interval.closed(1L, 3L), Interval.closed(5L, 8L)),
      expected = None,
      clue = "Merging two that do not overlap"
    )

  }

  test("example should work right") {
    ReadResource[IO]
      .readWith("day05-example.txt")(Inventory.parser)
      .map(Part2.mergeRanges)
      .map(Part2.count)
      .assertEquals(
        14L
      )
  }
