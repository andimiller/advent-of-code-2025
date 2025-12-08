package net.andimiller.aoc25
package day08

import cats.data.NonEmptyList
import cats.parse.*

case class Point(x: Int, y: Int, z: Int)

object Point:
  import SharedParsers.*
  val parser: Parser[Point] = for
    x <- int <* Parser.char(',')
    y <- int <* Parser.char(',')
    z <- int
  yield Point(x, y, z)

  val parserMany: Parser[NonEmptyList[Point]] =
    parser.repSep(newline)
