package net.andimiller.aoc25
package day07

import cats.data.NonEmptyList
import cats.parse.Parser as P
import cats.implicits.*

enum Cell:
  case Empty, Start, Splitter

object Cell:
  val parser = P.fromCharMap(
    Map(
      '.' -> Cell.Empty,
      'S' -> Cell.Start,
      '^' -> Cell.Splitter
    )
  )

case class Board(rows: NonEmptyList[Vector[Cell]])
object Board:
  val parser: P[Board] =
    Cell.parser.rep
      .map(_.toList.toVector)
      .repSep(SharedParsers.newline)
      .map(Board(_))
