package aoc

import aoc.Day15.{Grid, given}

class Test15 extends munit.FunSuite:

  private val sample: Grid =
    """
      |1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581
      |""".stripMargin.decoded

  private val input = entire[Grid](15)

  test("part 1 - sample") {
    assertEquals(sample.dijkstra, 40)
  }

  test("part 1 - result: " + input.dijkstra) {}

  test("part 2 - sample") {
    assertEquals(sample.times5.dijkstra, 315)
  }

  test("part 2 - result: " + input.times5.dijkstra) {}
