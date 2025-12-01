package net.andimiller.aoc25

import cats.implicits.*
import cats.effect.kernel.Async
import cats.parse.Parser
import fs2.io.file.{Files, Path}

trait ReadResource[F[_]]:
  def read(name: String): F[String]
  def readWith[T](name: String)(parser: Parser[T]): F[T]

object ReadResource:
  def apply[F[_]](using rr: ReadResource[F]): ReadResource[F] = rr

  given fromAsync[F[_]: Async]: ReadResource[F] = new ReadResource[F]:
    override def read(name: String): F[String] =
      Files[F].readUtf8(Path("./src/main/resources") / name).compile.string

    override def readWith[T](name: String)(parser: Parser[T]): F[T] =
      read(name).flatMap { str =>
        Async[F].fromEither(
          parser.parseAll(str).leftMap(_.show).leftMap(new Throwable(_))
        )
      }
