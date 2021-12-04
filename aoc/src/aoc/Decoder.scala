package aoc

trait Decoder[T]:

  private val outer = this

  def decode(s: String): Either[String, T]

  def map[U](f: T => U): Decoder[U] =
    new Decoder[U]:
      def decode(s: String): Either[String, U] =
        outer.decode(s).map(f)

  extension (s: String)
    def decoded: T =
      decode(s) match
        case Right(r)    => r
        case Left(error) => throw new IllegalArgumentException(s"Invalid input: $error")

object Decoder:

  given Decoder[String] with
    def decode(s: String): Either[String, String] = Right(s)

  given Decoder[Int] with
    def decode(s: String): Either[String, Int] = s.toIntOption.toRight(s"$s is not a integer")

  /** Null-safe string splitter.
    *
    * @param regex
    *   the separator regex
    * @return
    *   a vector of tokens, trimmed
    */
  extension (s: String)
    def splitTrim(regex: String): Vector[String] =
      val tokens = s.split(regex).nn
      tokens.toVector.flatMap[String] {
        case s: String => Some(s.trim.nn)
        case _         => None
      }
