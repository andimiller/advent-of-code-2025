package net.andimiller.aoc25
package day08

import cats.effect.std.Console
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import java.lang.Math.{abs, pow}

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def link(gpi: GridPointIndex): Option[(Point, Point)] = {
    val pairs                = gpi.findClosestPairs(0)
    var pair: (Point, Point) = null
    pairs.foldLeftM(gpi.points.map(Set(_)).toSet) { case (db, ((l, r), _)) =>
      val lp = db.find(_.contains(l)).get
      val rp = db.find(_.contains(r)).get

      pair = (l, r) // store for later

      val result = db - lp - rp + (lp ++ rp)
      if (result.size == 1)
        Left(result)
      else Right(result)
    }
    Option(pair)
  }

  def calculateAnswer(result: Option[(Point, Point)]): Long =
    result
      .map { case (l, r) =>
        l.x.toLong * r.x.toLong
      }
      .getOrElse(0L)

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day08-input.txt")(Point.parserMany)
        .bench("parse")
        .map { points =>
          new GridPointIndex(points.toList.toVector)
        }
        .map(link)
        .map(calculateAnswer)
        .flatTap(Console[F].println(_))
        .void
