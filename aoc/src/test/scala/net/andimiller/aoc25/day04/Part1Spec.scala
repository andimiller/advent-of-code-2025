package net.andimiller.aoc25.day04

import cats.implicits.*
import cats.effect.IO
import munit.CatsEffectSuite
import net.andimiller.aoc25.ReadResource

class Part1Spec extends CatsEffectSuite:

  test("neighbours should work right") {
    ReadResource[IO]
      .readWith("day04-tiny.txt")(Grid.parser)
      .map { g =>
        g.neighbours(1, 1)
      }
      .assertEquals(
        LazyList(
          Tile.Empty,
          Tile.Paper,
          Tile.Empty,
          Tile.Paper,
          Tile.Paper,
          Tile.Empty,
          Tile.Paper,
          Tile.Empty
        )
      )
  }

  test("example should work right") {
    (
      ReadResource[IO].readWith("day04-example.txt")(Grid.parser),
      ReadResource[IO].readWith("day04-example-expected.txt")(Grid.parser)
    ).flatMapN { (input, expected) =>
      IO {
        Part1.transformGrid(input)
      }.assertEquals(expected)
    }
  }
