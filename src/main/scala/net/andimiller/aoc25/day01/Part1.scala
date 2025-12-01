package net.andimiller.aoc25
package day01

import cats.data.{NonEmptyList, Writer}
import cats.effect.std.Console
import cats.effect.{IO, IOApp, Sync}
import cats.parse.Parser
import cats.implicits.*

import scala.annotation.tailrec

enum Turn:
  case Left(n: Int)
  case Right(n: Int)

  def expand: List[Turn] =
    this match
      case Turn.Left(n)  =>
        List.fill(n)(Turn.Left(1))
      case Turn.Right(n) =>
        List.fill(n)(Turn.Right(1))

object Turn:
  def parser: Parser[Turn] =
    for
      direction <- Parser.char('L').as('L').orElse(Parser.char('R').as('R'))
      count     <- Parser.charsWhile(_.isDigit).map(_.toInt)
    yield direction match
      case 'L' => Left(count)
      case 'R' => Right(count)

opaque type Dial = Int
object Dial:
  @tailrec
  private def cap(i: Int): Int =
    if (i < 0)
      cap(i + 100)
    else if (i > 99)
      cap(i - 100)
    else i

  private def cap2(i: Int): Writer[Int, Int] =
    if (i < 0)
      cap2(i + 100)
    else if (i > 99)
      cap2(i - 100)
    else if (i == 0)
      Writer.tell(1).as(i)
    else Writer.value(i)

  extension (d: Dial)
    def turn(t: Turn): Dial               = t match
      case Turn.Left(n)  => cap(d - n)
      case Turn.Right(n) => cap(d + n)
    def turn2(t: Turn): Writer[Int, Dial] = t match
      case Turn.Left(n)  => cap2(d - n)
      case Turn.Right(n) => cap2(d + n)

    def toInt: Int = d

  def create: Dial     = 50
  def of(i: Int): Dial = cap(i)

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  val fileParser: Parser[NonEmptyList[Turn]] = Turn.parser.repSep(SharedParsers.newline)

  def getSteps(turns: List[Turn]): List[Dial] =
    turns.scanLeft(Dial.create) { case (d, t) =>
      d.turn(t)
    }

  def countZeroes(turns: List[Turn]): Int =
    getSteps(turns).count(_.toInt == 0)

  def program[F[_]: {Sync, Console, ReadResource}] =
    ReadResource[F]
      .readWith("./day01-part1.txt")(fileParser)
      .map(steps => countZeroes(steps.toList))
      .flatTap(count => Console[F].println(s"counted $count zeroes"))
      .void

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] = program[IO]

  def countTicks(turns: List[Turn]): Int =
    turns
      .flatMap(_.expand)
      .foldLeftM(Dial.create) { case (d, t) =>
        d.turn2(t)
      }
      .run
      ._1

  def program[F[_]: {Sync, Console, ReadResource}] =
    ReadResource[F]
      .readWith("./day01-part1.txt")(Part1.fileParser)
      .map(steps => countTicks(steps.toList))
      .flatTap(count => Console[F].println(s"counted $count ticks"))
      .void
