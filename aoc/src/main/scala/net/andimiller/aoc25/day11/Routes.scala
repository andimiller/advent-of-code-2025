package net.andimiller.aoc25
package day11

import cats.parse.Parser
import cats.implicits.*
import cats.data.NonEmptyList

type Label = String
object Label:
  val start: Label = "you"
  val end: Label   = "out"

case class Routes(routes: Map[Label, NonEmptyList[Label]])

object Routes:
  val parser: Parser[Routes] = {
    val label: Parser[Label]                        = Parser.charsWhile(_.isLetter)
    val route: Parser[(Label, NonEmptyList[Label])] =
      for
        from <- label <* Parser.char(':') <* Parser.char(' ')
        to   <- label.repSep(Parser.char(' '))
      yield from -> to
    route.repSep(SharedParsers.newline).map { rs =>
      Routes(rs.toList.toMap)
    }
  }
