package net.andimiller.aoc25
package day02

import cats.data.NonEmptyList
import cats.parse.Parser

case class Ranges(contents: NonEmptyList[(Long, Long)])
object Ranges:
  private val int                         = Parser.charsWhile(_.isDigit).map(_.toLong)
  private val range: Parser[(Long, Long)] = for
    from <- int
    _    <- Parser.char('-')
    to   <- int
  yield (from, to)

  val parser: Parser[Ranges] = range.repSep(Parser.char(',')).map(Ranges(_))
