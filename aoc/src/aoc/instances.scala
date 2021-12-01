package aoc

import leopards.Semigroup

object instances:

  given [T](using Semigroup[T]): Semigroup[Option[T]] with
    extension (x: Option[T])
      def |+|(y: Option[T]) = (x, y) match
        case (Some(a), Some(b)) => Some(a |+| b)
        case (Some(a), None)    => x
        case (None, Some(b))    => y
        case _                  => None
