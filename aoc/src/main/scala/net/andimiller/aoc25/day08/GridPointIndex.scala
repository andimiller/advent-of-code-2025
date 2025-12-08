package net.andimiller.aoc25.day08

import Math.abs
import cats.implicits.*

case class GridPointIndex(points: Vector[Point], cells: Int = 300000) {
  def range(f: Point => Int): (Int, Int) = {
    val ps = points.map(f)
    (ps.min, ps.max)
  }

  val xrange: (Int, Int) = range(_.x)
  val yrange: (Int, Int) = range(_.y)
  val zrange: (Int, Int) = range(_.z)

  def cell(p: Point): (Int, Int, Int) =
    (p.x / cells, p.y / cells, p.z / cells)

  lazy val db: Map[(Int, Int, Int), Vector[Point]] =
    points.groupBy(cell).withDefault(_ => Vector.empty)

  def distanceSquared(p1: Point)(p2: Point): Long =
    (
      Math.pow(abs(p1.x - p2.x), 2) +
        Math.pow(abs(p1.y - p2.y), 2) +
        Math.pow(abs(p1.z - p2.z), 2)
    ).toLong

  private def shell(cx: Int, cy: Int, cz: Int)(radius: Int): Seq[(Int, Int, Int)] =
    for {
      dx <- -radius to radius
      dy <- -radius to radius
      dz <- -radius to radius
      if dx.abs + dy.abs + dz.abs == radius
    } yield (cx + dx, cy + dy, cz + dz)

  def spiralOut(center: (Int, Int, Int)): LazyList[(Int, Int, Int)] = {
    val (cx, cy, cz) = center
    LazyList.from(0).flatMap(shell(cx, cy, cz)(_))
  }

  def kNearest(query: Point, k: Int): LazyList[(Point, Long)] = {
    val (cx, cy, cz) = cell(query)

    spiralOut(cz, cy, cz)
      .map(db)
      .flatMap { ps =>
        ps
          .filter(_ != query)
          .map { p =>
            p -> distanceSquared(query)(p)
          }
          .sortBy(_._2)
          .to(LazyList)
      }
      .take(k)
  }

  def findClosestPairs(k: Int): LazyList[((Point, Point), Long)] = {
    def orderedPair(p1: Point, p2: Point): (Point, Point) =
      if (p1.## < p2.##) (p1, p2) else (p2, p1)

    val ll = LazyList
      .from(0, cells)
      .flatMap { radius =>
        points
          .to(LazyList)
          .flatMap { p1 =>
            val (cx, cy, cz) = cell(p1)

            shell(cz, cy, cz)(radius)
              .flatMap(db)
              .filter(_ != p1)
              .map { p2 =>
                orderedPair(p1, p2) -> distanceSquared(p1)(p2)
              }
              .distinctBy(_._1)
              .to(LazyList)
          }
          .distinctBy(_._1)
          .sortBy(_._2)
      }

    if (k > 0)
      ll.take(k)
    else ll
  }

}
