package net.andimiller.aoc25
package day03

import cats.data.NonEmptyList
import cats.parse.*

case class BatteryBank(banks: NonEmptyList[Int])

object BatteryBank:
  private val digit                            = Parser.fromStringMap((1 to 9).map(i => i.toString -> i).toMap)
  val bank: Parser[BatteryBank]                = digit.rep.map(BatteryBank(_))
  val banks: Parser[NonEmptyList[BatteryBank]] = bank.repSep(SharedParsers.newline)
