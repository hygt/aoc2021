package aoc

import aoc.Decoder.given

class Test16 extends munit.FunSuite:

  private val sample: HexString = "A0016C880162017C3686B18A3D4780".decoded

  private val input = entire[HexString](16)

  test("part 1 - sample") {
    assertEquals(Day16.part1(sample), 31)
  }

  test("part 1 - result: " + Day16.part1(input)) {}

  test("part 2 - result: " + Day16.part2(input)) {}
