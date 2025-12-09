package net.andimiller.aoc25.day09

// represents an inclusive range
case class TileRange(y: Long, x1: Long, x2: Long) {
  lazy val minX = (x1.min(x2))
  lazy val maxX = (x1.max(x2))

  def contains(t: Tile): Boolean =
    t.y == y && (t.x >= minX && t.x <= maxX)

  def contains(other: TileRange): Boolean =
    y == other.y &&
      other.minX >= minX &&
      other.maxX <= maxX
}
