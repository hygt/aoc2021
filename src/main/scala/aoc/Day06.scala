package aoc

import cats.data.State
import cats.instances.seq.*
import cats.syntax.foldable.*

object Day06:

  type Fish = Vector[Int]

  /** Immutable fish counter */
  opaque type FishCounter = IArray[Long]

  private def slow(i: Int): State[Vector[Int], Unit] = State.modify { fish =>
    fish
      .map(_ - 1)
      .flatMap:
        case -1 => Vector(6, 8)
        case n  => Vector(n)
  }

  private def fast(yesterday: Int): State[FishCounter, Unit] = State.modify { counter =>
    val mature = counter(yesterday % 9)
    val six    = (yesterday + 7) % 9
    counter.updated(six, counter(six) + mature)
  }

  def naive(fish: Fish, n: Int): Int =
    Seq.range(0, n).traverse_(slow).runS(fish).value.size

  def process(counter: FishCounter, n: Int): Long =
    Seq.range(0, n).traverse_(fast).runS(counter).value.sum

  given Decoder[FishCounter] = summon[Decoder[Vector[Int]]].map { fish =>
    val counter = new Array[Long](9)
    (1 to 8).foreach { day =>
      counter(day) = fish.count(_ == day)
    }
    IArray.unsafeFromArray(counter)
  }
