package net.andimiller.aoc25

import cats.parse.Parser

object SharedParsers:
  val int: Parser[Int]      = Parser.charsWhile(_.isDigit).map(_.toInt)
  val long: Parser[Long]    = Parser.charsWhile(_.isDigit).map(_.toLong)
  val newline: Parser[Unit] = Parser.char('\n').orElse(Parser.string("\r\n"))
