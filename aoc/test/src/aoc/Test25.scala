package aoc

import aoc.Day25.{Grid, given}

class Test25 extends munit.FunSuite:

  private val sample: Grid = """
    |v...>>.vv>
    |.vv>>.vv..
    |>>.>v>...v
    |>>v>>.>.v.
    |v>v.vv.v..
    |>.>>..v...
    |.vv..>.>v.
    |v.v..>>v.v
    |....v..v.>
  """.stripMargin.decoded

  private val input = entire[Grid](25)

  test("part 1 - sample") {
    assertEquals(sample.run(), 58)
  }

  test("part 1 - result: " + input.run()) {}
