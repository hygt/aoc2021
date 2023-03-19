package aoc

import aoc.Day18.{Nested, given}

class Test18 extends munit.FunSuite:

  private val sample = decode[Nested] {
    """
      |[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
      |""".stripMargin
  }

  private val input = load[Nested](18)

  test("part 0 - split") {
    val n: Nested = "[[[[0,7],4],[15,[0,13]]],[1,1]]".decoded
    val s: Nested = "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]".decoded
    assertEquals(n.split, s -> true)
  }

  test("part 0 - explode") {
    val n: Nested = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]".decoded
    val (m, _)    = n.explode
    assertEquals(m.explode._1, "[[[[0,7],4],[15,[0,13]]],[1,1]]".decoded: Nested)
  }

  test("part 0 - reduce") {
    val a: Nested = "[[[[4,3],4],4],[7,[[8,4],9]]]".decoded
    val b: Nested = "[1,1]".decoded
    assertEquals((a + b).reduce, "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]".decoded: Nested)
    assertEquals(Day18.reduce(Seq(a, b)), (a + b).magnitude)
  }

  test("part 1 - sample") {
    assertEquals(Day18.reduce(sample), 4140)
  }

  test("part 1 - result: " + Day18.reduce(input)) {}

  test("part 2 - sample") {
    assertEquals(Day18.largest(sample), 3993)
  }

  test("part 2 - result: " + Day18.largest(input)) {}
