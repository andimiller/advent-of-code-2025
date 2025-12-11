package net.andimiller.aoc25.day10

import cats.Semigroup
import cats.data.Ior
import cats.implicits.*

import scala.collection.BitSet

opaque type Joltage = Vector[Int]

object Joltage:
  val empty: Joltage = Vector.empty
  
  def fromBitSet(b: BitSet): Joltage =
    (0 to b.maxOption.getOrElse(0)).iterator.map { i =>
      if (b(i)) 1 else 0
    }.toVector
    
  def fromSeq(s: Seq[Int]): Joltage =
    s.toVector
    
  given Semigroup[Joltage] with
    override def combine(x: Joltage, y: Joltage): Joltage =
      x.alignWith(y):
        case Ior.Left(a)    => a
        case Ior.Right(b)   => b
        case Ior.Both(a, b) => a+b

  extension (j: Joltage)
    def breaks(goal: Joltage): Boolean =
      j.padTo(goal.size, 0).zip(goal).exists { case (c, g) =>
        c > g
      }

    def -(other: Joltage): Joltage =
      j.zipAll(other, 0, 0).map { case (a, b) => a - b }

    def hasNegative: Boolean =
      j.exists(_ < 0)      
      
    def toVector: Vector[Int] =
      j
