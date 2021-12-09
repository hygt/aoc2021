package aoc

import aoc.Day09.given

class Test09 extends munit.FunSuite:

  private val sample = decode[Vector[Int]] {
    """
      |2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678
      |""".stripMargin
  }

  private val input = load[Vector[Int]](9)

  test("part 1 - sample") {
    assertEquals(Day09.risk1(sample), 15)
  }

  test("part 1 - result: " + Day09.risk1(input)) {}

  test("part 2 - sample") {
    assertEquals(Day09.risk2(sample), 1134)
  }

  test("part 2 - result: " + Day09.risk2(input)) {}
