package net.andimiller.aoc25
package day04

import cats.{Applicative, Eval, Functor, Monad, Show, Traverse}
import cats.implicits.*
import cats.parse.Parser

opaque type Grid[T] = Vector[Vector[T]]

object Grid:
  extension [T](g: Grid[T])
    inline def unsafeGet(x: Int, y: Int): T              =
      g(y)(x)
    inline def get_(x: Int, y: Int): Option[T]           =
      (g: Vector[Vector[T]]).get(y).flatMap(_.get(x))
    inline def neighbours(x: Int, y: Int): LazyList[T]   =
      for
        yi <- LazyList.range(y - 1, y + 2)
        xi <- LazyList.range(x - 1, x + 2)
        r  <-
          if (yi == y && xi == x)
            LazyList.empty
          else
            LazyList.from(get_(xi, yi))
      yield r
    inline def zipWithCoordinates: Grid[((Int, Int), T)] =
      g.zipWithIndex.map { (r, y) =>
        r.zipWithIndex.map { (t, x) =>
          (x, y) -> t
        }
      }
    inline def iterator_ : Iterator[T]                   = g.iterator.flatMap(_.iterator)

  given Functor[Grid] with Traverse[Grid]:
    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
      (fa: Vector[Vector[A]]).map(_.map(f))

    override def foldLeft[A, B](fa: Grid[A], b: B)(f: (B, A) => B): B =
      (fa: Vector[Vector[A]]).flatten.foldLeft(b)(f)

    override def foldRight[A, B](fa: Grid[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      (fa: Vector[Vector[A]]).flatten.foldRight(lb)(f)

    override def traverse[G[_]: Applicative, A, B](fa: Grid[A])(f: A => G[B]): G[Grid[B]] =
      (fa: Vector[Vector[A]]).traverse { row =>
        row.traverse { a =>
          f(a)
        }
      }

  given [T: Show]: Show[Grid[T]] = Show.show { g =>
    g.iterator.map(_.mkString_("")).mkString(System.lineSeparator())
  }

  val parser: Parser[Grid[Tile]] =
    Tile.parser.rep.map(_.iterator.toVector).repSep(SharedParsers.newline).map(_.iterator.toVector)
