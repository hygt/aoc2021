package aoc

import aoc.Decoder.splitTrim
import cats.instances.map.*
import cats.instances.set.*
import cats.syntax.foldable.*

import scala.util.control.NonFatal

object Day12:

  enum Node:
    case Start
    case End
    case Small(s: String)
    case Big(s: String)

  object Node:
    def apply(s: String): Node = s match
      case "start" => Start
      case "end"   => End
      case _ =>
        if s.toLowerCase == s then Small(s)
        else if s.toUpperCase == s then Big(s)
        else throw new IllegalArgumentException(s"unrecognized node $s")

  opaque type Graph = Map[Node, Set[Node]]

  def process1(graph: Graph): Int =
    def dfs(node: Node, visited: Set[Node]): Int = node match
      case Node.End => 1
      case _: Node.Big =>
        val next = graph.get(node).toSeq.flatMap(_.diff(visited))
        next.map { n => dfs(n, visited) }.sum
      case _ => // Start or Small
        val next = graph.get(node).toSeq.flatMap(_.diff(visited))
        next.map { n => dfs(n, visited + node) }.sum

    dfs(Node.Start, Set.empty)

  def process2(graph: Graph): Int =
    def next(node: Node): Seq[Node] =
      graph.get(node).toSeq.flatten.filterNot(_ == Node.Start)

    def dfs(node: Node, visited: Set[Node], twice: Boolean): Int = node match
      case Node.End => 1
      case _: Node.Small =>
        if !visited(node) then next(node).map { n => dfs(n, visited + node, twice) }.sum
        else if !twice then next(node).map { n => dfs(n, visited, true) }.sum
        else 0
      case _ => // Start or Big
        next(node).map { n => dfs(n, visited, twice) }.sum

    dfs(Node.Start, Set.empty, false)

  given Decoder[Graph] with
    def decode(s: String): Either[String, Graph] = try
      val edges = s
        .splitTrim("""\n""")
        .foldMap:
          case s"$x-$y" =>
            Map(
              Node(x) -> Set(Node(y)),
              Node(y) -> Set(Node(x))
            )
          case line => throw new IllegalArgumentException(s"unrecognized edge $line")
      Right(edges)
    catch case NonFatal(e) => Left(e.getMessage.nn)
