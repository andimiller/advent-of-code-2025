package net.andimiller.aoc25
package day06

import cats.data.NonEmptyList
import cats.parse.Parser

enum Operation:
  case Add, Multiply

case class Problem(values: NonEmptyList[Long], operation: Operation)
case class Worksheet(problems: NonEmptyList[Problem])

object Worksheet:
  val parser: Parser[Worksheet] = {
    import SharedParsers.*
    val op: Parser[Operation] = Parser.fromCharMap(Map('*' -> Operation.Multiply, '+' -> Operation.Add))
    val skipSpaces            = Parser.charsWhile0(_ == ' ')
    val numberLine            = (long.surroundedBy(skipSpaces).rep <* newline).backtrack
    val operations            = op.surroundedBy(skipSpaces).rep
    for
      ns  <- numberLine.rep
      ops <- operations
    yield Worksheet(
      ops.zipWithIndex.map { case (o, idx) =>
        Problem(ns.map(_.toList(idx)), o)
      }
    )
  }
