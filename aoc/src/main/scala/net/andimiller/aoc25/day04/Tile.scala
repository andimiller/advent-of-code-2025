package net.andimiller.aoc25.day04

import cats.Show
import cats.parse.Parser

enum Tile:
  case Paper, Empty, ToBeRemoved

object Tile:
  given Show[Tile] = Show.show:
    case Tile.Paper       => "@"
    case Tile.Empty       => "."
    case Tile.ToBeRemoved => "x"

  val parser: Parser[Tile] =
    Parser.fromCharMap:
      Map(
        '@' -> Tile.Paper,
        '.' -> Tile.Empty,
        'x' -> Tile.ToBeRemoved
      )
