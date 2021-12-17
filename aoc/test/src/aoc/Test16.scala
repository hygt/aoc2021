package aoc

import scodec.bits.BitVector

class Test16 extends munit.FunSuite:

  private val input = entire[String](16)

  test("part 1 - sample") {
    assertEquals(Day16.part1("A0016C880162017C3686B18A3D4780"), 31)
  }

  test("part 1 - result: " + Day16.part1(input)) {}

  test("part 2 - result: " + Day16.part2(input)) {}
