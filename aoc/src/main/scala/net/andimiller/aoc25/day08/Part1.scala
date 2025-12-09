package net.andimiller.aoc25
package day08

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*
import Math.{pow, abs}

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def link(gpi: PointDb)(k: Int): Set[Set[Point]] = {
    val pairs = gpi.findClosestPairs(k).toVector
    pairs.foldLeft(gpi.points.map(Set(_)).toSet) { case (db, ((l, r), _)) =>
      val lp = db.find(_.contains(l)).get
      val rp = db.find(_.contains(r)).get

      db - lp - rp + (lp ++ rp)
    }
  }

  def calculateAnswer(groups: Set[Set[Point]]): Int =
    groups.toVector.map(_.size).sorted.reverse.take(3).product

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day08-input.txt")(Point.parserMany)
        .bench("parse")
        .map { points =>
          new PointDb(points.toList.toVector)
        }
        .map(link(_)(1000))
        .map(calculateAnswer)
        .flatTap(Console[F].println(_))
        .void
