package net.andimiller.aoc25.day10

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.*
import munit.CatsEffectSuite
import net.andimiller.aoc25.day08.Part1.{calculateAnswer, link}

import scala.math.Ordered.orderingToOrdered

class Part2Spec extends CatsEffectSuite {

  test("Joltage semigroup") {
    assertEquals(
      obtained =
        (Joltage.fromSeq(Vector(0,1,1)) |+| Joltage.fromSeq(Vector(1,0,1,1))),
      expected =
        Joltage.fromSeq(Vector(1,1,2,1))
    )
    assertEquals(
      obtained =
        (Joltage.fromSeq(Vector(0, 1, 1)) |+| Joltage.empty),
      expected =
        Joltage.fromSeq(Vector(0, 1, 1))
    )
  }
  
  test("Joltage breaks") {
    assert(
      !(Joltage.empty breaks Joltage.empty)
    )
    assert(
      (Joltage.fromSeq(Vector(1)) breaks Joltage.fromSeq(Vector(0)))
    )
    assert(
      !(Joltage.fromSeq(Vector(0)) breaks Joltage.fromSeq(Vector(1)))
    )
    assert(
      (Joltage.fromSeq(Vector(0,0,600)) breaks Joltage.fromSeq(Vector(0,0,10)))
    )
    assert(
      (Joltage.fromSeq(Vector(0, 600)) breaks Joltage.fromSeq(Vector(0, 10, 0)))
    )
    
  }
  
}
