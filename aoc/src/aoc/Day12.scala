package aoc

import aoc.Decoder.splitTrim
import cats.instances.map.*
import cats.instances.set.*
import cats.syntax.foldable.*

import scala.collection.mutable
import scala.util.control.NonFatal

object Day12:

  enum Node:
    case Start
    case End
    case Small(s: String)
    case Big(s: String)

  object Node:
    def apply(s: String): Node = s match {
      case "start" => Start
      case "end"   => End
      case _ =>
        if s.toLowerCase == s then Small(s)
        else if s.toUpperCase == s then Big(s)
        else throw new IllegalArgumentException(s"unrecognized node $s")
    }

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
    def dfs(node: Node, visited: Set[Node], twice: Boolean, path: Seq[Node]): Set[Seq[Node]] = node match
      case Node.End => Set(path :+ node)
      case Node.Start =>
        val next = graph.get(node).toSet.flatten
        next.flatMap { n => dfs(n, Set(Node.Start), false, Seq(Node.Start)) }
      case _: Node.Big =>
        val next = graph.get(node).toSet.flatMap(_.diff(visited))
        next.flatMap { n => dfs(n, visited, twice, path :+ node) }
      case _: Node.Small =>
        val next = graph.get(node).toSet.flatMap(_.diff(visited))
        if twice then next.flatMap { n => dfs(n, visited + node, twice, path :+ node) }
        else // we need to branch off here
          next.flatMap { n => dfs(n, visited, true, path :+ node) } ++
            next.flatMap { n => dfs(n, visited + node, false, path :+ node) }

    dfs(Node.Start, Set.empty, false, Seq.empty).size

  given Decoder[Graph] with
    def decode(s: String): Either[String, Graph] = try
      val edges = s.splitTrim("""\n""").foldMap {
        case s"$x-$y" =>
          Map(
            Node(x) -> Set(Node(y)),
            Node(y) -> Set(Node(x))
          )
        case line => throw new IllegalArgumentException(s"unrecognized edge $line")
      }
      Right(edges)
    catch case NonFatal(e) => Left(e.getMessage.nn)
