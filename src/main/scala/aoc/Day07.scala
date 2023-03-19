package aoc

object Day07:

  def cheapest1(xs: Vector[Int]): Int =
    val sorted = xs.sorted
    val len    = sorted.size
    val median =
      if len % 2 == 0 then (sorted(len / 2 - 1) + sorted(len / 2)) / 2
      else sorted(len / 2)
    sorted.foldLeft(0) { (acc, pos) => acc + (pos - median).abs }

  private def fuel(distance: Int): Int =
    distance * (distance + 1) / 2

  private def consumption(xs: Vector[Int], target: Int): Int =
    xs.foldLeft(0) { (acc, pos) => acc + fuel((pos - target).abs) }

  // Why the mean ± 0.5 is the answer:
  // https://www.reddit.com/r/adventofcode/comments/rawxad/2021_day_7_part_2_i_wrote_a_paper_on_todays/
  def cheapest2(xs: Vector[Int]): Int =
    val mean = xs.foldLeft(0.0) { (acc, x) => acc + x.toDouble } / xs.size
    consumption(xs, mean.toInt) min consumption(xs, mean.ceil.toInt)
