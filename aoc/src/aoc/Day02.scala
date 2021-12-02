package aoc

import cats.data.State
import cats.implicits.*

object Day02:

  enum Command:
    case Up(v: Int)
    case Down(v: Int)
    case Forward(v: Int)

  /** @param x
    *   horizontal position
    * @param y
    *   depth
    * @param a
    *   aim
    */
  case class Position(x: Int, y: Int, a: Int) {
    def result: Int = x * y
  }

  final val Zero = Position(0, 0, 0)

  private def next1(cmd: Command): State[Position, Unit] = cmd match
    case Command.Up(v)      => State.modify { case Position(x, y, _) => Position(x, y - v, 0) }
    case Command.Down(v)    => State.modify { case Position(x, y, _) => Position(x, y + v, 0) }
    case Command.Forward(v) => State.modify { case Position(x, y, _) => Position(x + v, y, 0) }

  private def next2(cmd: Command): State[Position, Unit] = cmd match
    case Command.Up(v)      => State.modify { case Position(x, y, a) => Position(x, y, a - v) }
    case Command.Down(v)    => State.modify { case Position(x, y, a) => Position(x, y, a + v) }
    case Command.Forward(v) => State.modify { case Position(x, y, a) => Position(x + v, y + a * v, a) }

  private def process(next: Command => State[Position, Unit])(commands: Seq[Command]): Int =
    commands.traverse_(next).runS(Zero).value.result

  def process1: Seq[Command] => Int = process(next1)

  def process2: Seq[Command] => Int = process(next2)
