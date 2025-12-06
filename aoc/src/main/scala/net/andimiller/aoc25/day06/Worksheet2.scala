package net.andimiller.aoc25
package day06

import cats.data.NonEmptyList
import cats.parse.*
import cats.syntax.all.*
import cats.implicits.*

object Worksheet2:
  val parser: Parser[Worksheet] = {
    import SharedParsers.*

    val operator     = Parser.fromCharMap(Map('*' -> Operation.Multiply, '+' -> Operation.Add))
    val paddedNumber = (digit | space).rep.string.flatMap { s =>
      s.filter(_.isDigit).toLongOption match
        case Some(value) => Parser.pure(value)
        case None        => Parser.failWith("not a valid int")
    }

    val header =
      for
        number <- paddedNumber
        op     <- operator
        _      <- newline
      yield Problem(NonEmptyList.of(number), op)

    val extraNumbers =
      (paddedNumber <* newline).backtrack

    val separator =
      space.rep <* newline

    val problem = for
      h  <- header
      ns <- extraNumbers.rep
    yield h.copy(values = h.values.prependList(ns.toList))

    problem.repSep(separator).map(Worksheet)
  }
