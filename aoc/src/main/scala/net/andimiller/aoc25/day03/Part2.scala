package net.andimiller.aoc25
package day03

import cats.effect.std.Console
import cats.effect.{Clock, IO, IOApp, Async}
import cats.implicits.*

import scala.collection.mutable

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def findMaxJoltage(bb: BatteryBank, solutionLength: Int = 12): Long = {
    var removals = bb.banks.size - solutionLength

    val stack = mutable.Stack.empty[Int]
    for (i <- bb.banks.iterator) {
      while (removals > 0 && stack.nonEmpty && i > stack.top) {
        stack.pop()
        removals -= 1
      }
      stack.push(i)
    }
    stack.toVector.reverse.take(solutionLength).mkString.toLong
  }

  def program[F[_]: {Async, Console, ReadResource, Clock}] =
    ReadResource[F]
      .readWith("./day03-input.txt")(BatteryBank.banks)
      .flatMap { banks =>
        Bench[F].bench(Async[F].blocking { banks.map(findMaxJoltage(_)).combineAll }).flatTap { total =>
          Console[F].println(total)
        }
      }
      .void
