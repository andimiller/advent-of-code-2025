package net.andimiller.aoc25
package day10

import cats.Monad
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.std.{Console, Random}
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import scala.collection.BitSet


object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def distance(m: Machine)(buttons: Set[Int]): Int = {
    val result = buttons.map(m.buttons).foldLeft(BitSet()) {  _ ^ _  }
    (result ^ m.target).size
  }
  
  def solveRaw(m: Machine): Set[Int] = {
    val buttonCount = m.buttons.size
    val indices = m.buttons.indices.toList
    (1 to buttonCount).iterator.flatMap { i =>
      indices.combinations(i).map(_.toSet)
    }.find { buttons =>
      distance(m)(buttons) == 0
    }.get
  }
    

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench, Random}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day10-input.txt")(Machine.machines)
        .bench("parse")
        .flatMap { machines =>
          blocking {
            machines.map(solveRaw)
          }.bench("solve", iterations=2)
        }
        .flatMap { results =>
          blocking {
            results.toList.map(_.size).sum
          }
        }
        .flatTap(Console[F].println(_))
        .void
