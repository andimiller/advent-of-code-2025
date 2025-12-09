package net.andimiller.aoc25.day09

import cats.effect.IO
import cats.implicits.*
import munit.CatsEffectSuite
import net.andimiller.aoc25.day08.Part1.{calculateAnswer, link}

class Part1Spec extends CatsEffectSuite {

  Vector(
    (Tile(2, 5), Tile(11, 1)) -> 50L,
    (Tile(2, 5), Tile(9, 7))  -> 24L,
    (Tile(9, 7), Tile(2, 5))  -> 24L,
    (Tile(7, 1), Tile(11, 7)) -> 35L,
    (Tile(7, 3), Tile(2, 3))  -> 6L
  ).foreach { case ((t1, t2), size) =>
    test(s"$t1 <-> $t2 == $size") {
      assertEquals(
        obtained = Part1.rectangleSize(t1, t2),
        expected = size
      )
    }
  }

}
