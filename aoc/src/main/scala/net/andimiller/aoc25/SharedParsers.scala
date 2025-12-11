package net.andimiller.aoc25

import cats.parse.Parser

object SharedParsers:
  val digit: Parser[Char]   = Parser.charWhere(_.isDigit).withContext("digit")
  val space: Parser[Unit]   = Parser.char(' ').withContext("space")
  val int: Parser[Int]      = Parser.charsWhile(_.isDigit).map(_.toInt).withContext("int")
  val long: Parser[Long]    = Parser.charsWhile(_.isDigit).map(_.toLong).withContext("long")
  val newline: Parser[Unit] = Parser.char('\n').orElse(Parser.string("\r\n")).withContext("newline")