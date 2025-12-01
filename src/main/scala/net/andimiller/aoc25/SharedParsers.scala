package net.andimiller.aoc25

import cats.parse.Parser

object SharedParsers:
  val newline: Parser[Unit] = Parser.char('\n').orElse(Parser.string("\r\n"))
