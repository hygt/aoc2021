package aoc

import scala.util.control.NonFatal

class HexString private (value: Seq[Int]):
  def toBin: String =
    value
      .foldLeft(new StringBuilder) { (acc, x) =>
        val bin = x.toBinaryString
        if acc.isEmpty then acc ++= bin
        else
          val padding = "0" * (16 - bin.length)
          acc ++= padding
          acc ++= bin
        acc
      }
      .toString

  def toBoolSeq: Seq[Boolean] =
    toBin.map(_ == '1')

object HexString:
  given Decoder[HexString] with
    def decode(s: String): Either[String, HexString] = try
      if s.isBlank then Left("hex string cannot be empty")
      else
        val head = s.length % 4
        val seq =
          if head == 0 then s.grouped(4).toSeq
          else s.take(head) +: s.drop(head).grouped(4).toSeq
        Right(HexString(seq.map(s => Integer.parseUnsignedInt(s, 16))))
    catch case NonFatal(e) => Left(e.getMessage.nn)
