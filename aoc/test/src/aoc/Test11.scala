package aoc

import aoc.Day11.{Grid, given}

class Test11 extends munit.FunSuite:

  private val sample: Grid =
    """
      |5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526
      |""".stripMargin.decoded

  private val input = entire[Grid](11)

  test("part 1 - sample") {
    assertEquals(sample.run(100), 1656)
  }

  test("part 1 - result: " + input.run(100)) {}

  test("part 2 - sample") {
    assertEquals(sample.all(), 195)
  }

  test("part 2 - result: " + input.all()) {}
