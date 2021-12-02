package aoc

import aoc.Day02.Command

trait Decoder[T]:
  def decode(s: String): T

  extension (s: String) def decoded: T = decode(s)

object Decoder:

  given Decoder[String] with
    def decode(s: String): String = s

  given Decoder[Int] with
    def decode(s: String): Int = s.toInt

  given Decoder[Command] with
    def decode(s: String): Command = s.split(' ').toList match
      case "up" :: v :: Nil      => Command.Up(v.toInt)
      case "down" :: v :: Nil    => Command.Down(v.toInt)
      case "forward" :: v :: Nil => Command.Forward(v.toInt)
      case _                     => throw new IllegalArgumentException(s"Invalid input: $s")
