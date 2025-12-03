package net.andimiller.aoc25
package day03

import cats.implicits.*
import cats.effect.{IO, IOApp, Sync}
import cats.effect.std.Console

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def findMaxJoltage(bb: BatteryBank): Int = {
    def go(l: List[Int]): List[Int] = {
      l match
        case (_ :: Nil) | Nil =>
          List.empty
        case head :: tail     =>
          ((head * 10) + tail.max) :: go(tail)
    }
    go(bb.banks.toList).max
  }

  def program[F[_]: {Sync, Console, ReadResource}] =
    ReadResource[F]
      .readWith("./day03-input.txt")(BatteryBank.banks)
      .flatMap { banks =>
        val total = banks.map(findMaxJoltage).combineAll
        Console[F].println(total)
      }
      .void
