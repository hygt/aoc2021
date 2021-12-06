package aoc

import aoc.Day06.{*, given}

class Test06 extends munit.FunSuite:

  private def sample: FishCounter = "3,4,3,1,2".decoded

  private def input = entire[FishCounter](6)

  test("part 1 - sample") {
    assertEquals(Day06.process(sample, 18), 26L)
    assertEquals(Day06.process(sample, 80), 5934L)
  }

  test("part 1 - result: " + Day06.process(input, 80)) {}

  test("part 2 - sample") {
    assertEquals(Day06.process(sample, 256), 26984457539L)
  }

  test("part 2 - result: " + Day06.process(input, 256)) {}
