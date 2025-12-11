package net.andimiller.aoc25
package day10

import cats.implicits.*
import cats.data.NonEmptyList
import cats.parse.{Parser => P}

import scala.collection.BitSet

case class Machine(target: BitSet, buttons: Vector[BitSet], joltage: Joltage)

object Machine:
  import SharedParsers.*
  val target: P[BitSet] = (P.char('[') *> P.fromCharMap(Map('.' -> false, '#' -> true)).rep <* P.char(']')).map { bools =>
    BitSet.fromSpecific(
      bools.iterator.zipWithIndex.collect { case (true, i) => i}
    )
  }
  
  val button: P[BitSet] = (P.char('(') *> int.repSep(P.char(',')) <* P.char(')')).map(ints =>BitSet.fromSpecific(ints.iterator))
  val joltage: P[Joltage] = (P.char('{') *> int.repSep(P.char(',')) <* P.char('}')).map(_.toList.toVector).map(Joltage.fromSeq)
  
  val parser: P[Machine] = for 
    t <- target <* P.char(' ')
    ws <- button.repSep(P.char(' ')).map(_.toList.toVector) <* P.char(' ')
    j <- joltage
  yield Machine(t, ws, j)
  
  val machines: P[NonEmptyList[Machine]] = parser.repSep(newline)