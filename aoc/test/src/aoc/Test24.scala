package aoc

import aoc.Day24.{Instruction, given}

/** {{{
  *   # div check offset
  *   01   1    14     16
  *   02   1    11      3
  *   03   1    12      2
  *   04   1    11      7
  *   05  26   -10     13
  *   06   1    15      6
  *   07  26   -14     10
  *   08   1    10     11
  *   09  26    -4      6
  *   10  26    -3      5
  *   11   1    13     11
  *   12  26    -3      4
  *   13  26    -9      4
  *   14  26   -12      6
  *
  *   i5 = i4 - 3
  *   i7 = i6 - 8
  *   i9 = i8 + 7
  *   i10 = i3 - 1
  *   i12 = i11 + 8
  *   i13 = i2 - 6
  *   i14 = i1 + 4
  * }}}
  * @see
  *   https://github.com/dphilipson/advent-of-code-2021/blob/master/src/days/day24.rs
  */
class Test24 extends munit.FunSuite:

  private val sample = decode[Instruction] {
    """
      |inp w
      |add z w
      |mod z 2
      |div w 2
      |add y w
      |mod y 2
      |div w 2
      |add x w
      |mod x 2
      |div w 2
      |mod w 2
      |""".stripMargin
  }

  private val input = load[Instruction](24)

  test("part 1 - sample") {
    assertEquals(Day24.process(sample)(8L), Some(8L))
    assertEquals(Day24.process(sample)(9L), None)
  }

  test("part 1 - result") {
    assert(Day24.process(input)(59_996_912_981_939L).isDefined)
  }

  test("part 2 - result") {
    assert(Day24.process(input)(17_241_911_811_915L).isDefined)
  }
