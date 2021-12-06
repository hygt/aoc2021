package aoc

import aoc.Decoder.splitTrim
import cats.data.State
import cats.implicits.*

object Day06:

  opaque type Fish = Vector[Int]

  /** Mutable fish counter */
  opaque type FishCounter = Array[Long]

  private def slow(i: Int): State[Vector[Int], Unit] = State.modify { fish =>
    fish
      .map(_ - 1)
      .flatMap {
        case -1 => Vector(6, 8)
        case n  => Vector(n)
      }
  }

  private def fast(day: Int): State[FishCounter, Unit] = State.modify { counter =>
    val yesterday = day       % 9
    val today     = (day + 1) % 9

    val mature = counter(yesterday) // today's mature fish were yesterday's '1'
    counter((today + 6) % 9) += mature // mature fish become '6'

    counter
  }

  def naive(fish: Fish, n: Int): Int =
    Seq.range(0, n).traverse_(slow).runS(fish).value.size

  def process(counter: FishCounter, n: Int): Long =
    Seq.range(0, n).traverse_(fast).runS(counter).value.sum

  given Decoder[Fish] with
    def decode(s: String): Either[String, Fish] =
      s.splitTrim(",")
        .map(_.toIntOption)
        .sequence
        .toRight("failed to parse fish")

  given Decoder[FishCounter] = summon[Decoder[Fish]].map { fish =>
    val counter = new Array[Long](9)
    (1 to 8).foreach { day =>
      counter(day) = fish.count(_ == day)
    }
    counter
  }
