package net.andimiller.aoc25.day09

import cats.data.NonEmptyList
import cats.implicits.*

case class VerticalEdge(x: Long, minY: Long, maxY: Long, isEnter: Boolean)

case class TileTracker(redTiles: NonEmptyList[Tile]) {

  // shoelace formula
  lazy val signedArea: Long = {
    val pairs = redTiles.toList.zip(redTiles.tail.appended(redTiles.head))
    pairs.map { case (a, b) => a.x * b.y - b.x * a.y }.sum
  }

  lazy val insideIsRight: Boolean = signedArea < 0

  // there are lines of green tiles between each adjacent red tile
  lazy val verticalEdges: Vector[VerticalEdge] = {
    val pairs = redTiles.toList.zip(redTiles.tail.appended(redTiles.head))

    pairs.collect {
      case (a, b) if a.x == b.x =>
        val goingUp = b.y < a.y
        val isEnter = if (insideIsRight) goingUp else !goingUp
        VerticalEdge(a.x, math.min(a.y, b.y), math.max(a.y, b.y), isEnter)
    }.toVector
  }

  lazy val filledRanges: Map[Long, Vector[TileRange]] = {
    val minY = verticalEdges.map(_.minY).min
    val maxY = verticalEdges.map(_.maxY).max

    (minY to maxY).map { y =>
      val edgesAtY = verticalEdges
        .filter(e => e.minY <= y && y <= e.maxY)
        .sortBy(_.x)

      val enters = edgesAtY.filter(_.isEnter).map(_.x)
      val exits  = edgesAtY.filter(!_.isEnter).map(_.x)

      val ranges = enters.zip(exits).map { case (enter, exit) => TileRange(y, enter, exit) }

      y -> ranges
    }.toMap
  }

  def rectangleIsValid(a: Tile, b: Tile): Boolean = {
    val (minX, maxX) = (a.x.min(b.x), a.x.max(b.x))
    val (minY, maxY) = (a.y.min(b.y), a.y.max(b.y))

    (minY to maxY).forall { y =>
      filledRanges.get(y).exists { ranges =>
        ranges.exists(_.contains(TileRange(y, minX, maxX)))
      }
    }
  }

  def draw(): Unit = {
    (0 to redTiles.toList.map(_.y).max.toInt).foreach { y =>
      val s = (0 to redTiles.toList.map(_.x).max.toInt).toVector.map { x =>
        val r = redTiles.toList.contains(Tile(x, y))
        val f = filledRanges.get(y).exists(_.exists(_.contains(Tile(x, y))))
        if (r && f)
          "R"
        else if (r)
          "r"
        else if (f)
          "F"
        else "."
      }.mkString
      println(s)
    }
  }
}
