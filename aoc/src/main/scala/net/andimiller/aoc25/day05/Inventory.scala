package net.andimiller.aoc25
package day05

import cats.data.{NonEmptyList, NonEmptySet}
import cats.implicits.*
import cats.parse.Parser
import spire.math.Interval

case class Inventory(fresh: NonEmptyList[Interval[Long]], available: NonEmptySet[Long])

object Inventory:
  val parser: Parser[Inventory] =
    import SharedParsers._
    val range: Parser[Interval[Long]] =
      (long <* Parser.char('-'), long).mapN(Interval.closed)

    for
      freshRanges <- range.repSep(newline) <* newline <* newline
      available   <- long.repSep(newline)
    yield Inventory(freshRanges, available.toNes)
