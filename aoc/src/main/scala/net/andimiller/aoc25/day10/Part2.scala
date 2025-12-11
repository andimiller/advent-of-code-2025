package net.andimiller.aoc25
package day10

import cats.{Eval, Monad}
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.std.{Console, Random}
import cats.effect.{Async, Clock, IO, IOApp}
import cats.implicits.*
import net.andimiller.aoc25.Bench.syntax.*

import scala.collection.BitSet
import scala.math.Ordered.orderingToOrdered


object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]
  
  def minPresses(m: Machine):Int = {
    
    /*
    def go(runningTotal: Joltage, visited: List[Int]): Eval[Vector[List[Int]]] = {
      println(visited -> runningTotal)
      // check which buttons we could press that won't blow the joltage
      val validPresses = m.buttons.zipWithIndex.map { case (bs, idx) =>
        idx -> (Joltage.fromBitSet(bs) |+| runningTotal)
      }.filter { case (_, total) =>
        !(total breaks m.joltage)
      }
      
      // check which buttons hit our goal, and prepare them to be output
      val ends = validPresses.filter(_._2 == m.joltage).map { case (idx, _) =>
        idx :: visited
      }

      // and recurse if we're not done
      if (ends.nonEmpty)
        Eval.now(ends)
      else
        validPresses.traverse { case (idx, total) =>
          go(total, idx :: visited)
        }.map { results =>
          results.flatten ++ ends
        }
    }
     */
    
    def go(paths: Vector[(Joltage, List[Int])], visited: Set[Joltage]): Eval[Option[List[Int]]] = {
      if (paths.isEmpty) Eval.now(None)
      else {
        val nextButtons = paths.flatMap { case (runningTotal, path) =>
          m.buttons.zipWithIndex.collect {
            case (bs, idx) =>
              val total = Joltage.fromBitSet(bs) |+| runningTotal
              (idx, total)
          }.filter { case (_, total) =>
            !(total breaks m.joltage) && !visited(total)
          }.map { case (idx, total) =>
            (total, idx :: path)
          }
        }.distinctBy(_._1)
        
        nextButtons.find(_._1 == m.joltage) match {
          case (Some((_, path))) => Eval.now(Some(path))
          case None =>
            Eval.defer(
              go(nextButtons, visited ++ nextButtons.map(_._1))
            )
        }
      }
    }
    
    go(Vector((Joltage.empty, Nil)), Set(Joltage.empty)).value.fold(0)(_.size)
  }

    

  def program[F[_]: {Async, Console, ReadResource, Clock, Bench, Random}]: F[Unit] =
    gym:
      ReadResource[F]
        .readWith("./day10-input.txt")(Machine.machines)
        .bench("parse")
        .flatMap { machines =>
          blocking {
            machines.map(minPresses)
          }.bench("solve", iterations=1)
        }
        .flatTap(Console[F].println(_))
        .void
