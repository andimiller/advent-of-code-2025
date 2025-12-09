package net.andimiller.aoc25.day08

import Math.abs
import cats.implicits.*

case class PointDb(points: Vector[Point]) {

  private def distanceSquared(p1: Point)(p2: Point): Long =
    (
      Math.pow(abs(p1.x - p2.x), 2) +
        Math.pow(abs(p1.y - p2.y), 2) +
        Math.pow(abs(p1.z - p2.z), 2)
    ).toLong

  def findClosestPairs: LazyList[((Point, Point), Long)] =
    def orderedPair(p1: Point, p2: Point): (Point, Point) =
      if (p1.## < p2.##) (p1, p2) else (p2, p1)

    points
      .combinations(2)
      .map { case Vector(a, b) =>
        orderedPair(a, b)
      }
      .distinct
      .map { case (p1, p2) =>
        orderedPair(p1, p2) -> distanceSquared(p1)(p2)
      }
      .distinctBy(_._1)
      .toVector
      .sortBy(_._2)
      .to(LazyList)

}
