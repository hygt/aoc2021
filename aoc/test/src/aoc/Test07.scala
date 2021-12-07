package aoc

import aoc.Decoder.given

class Test07 extends munit.FunSuite:

  private val sample: Vector[Int] = "16,1,2,0,4,2,7,1,2,14".decoded

  private val input = entire[Vector[Int]](7)

  test("part 1 - sample") {
    assertEquals(Day07.cheapest1(sample), 37)
  }

  test("part 1 - result: " + Day07.cheapest1(input)) {}

  test("part 2 - sample") {
    assertEquals(Day07.cheapest2(sample), 168)
  }

  test("part 2 - result: " + Day07.cheapest2(input)) {}
