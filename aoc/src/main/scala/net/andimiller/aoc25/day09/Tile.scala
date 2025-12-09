package net.andimiller.aoc25
package day09

import cats.data.NonEmptyList
import cats.parse.Parser

case class Tile(x: Long, y: Long)

object Tile:
  import SharedParsers.*

  val parser: Parser[Tile]              = {
    for
      x <- long <* Parser.char(',')
      y <- long
    yield Tile(x, y)
  }
  val tiles: Parser[NonEmptyList[Tile]] = parser.repSep(newline)
