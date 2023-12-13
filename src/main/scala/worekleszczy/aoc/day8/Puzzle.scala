package worekleszczy.aoc.day8

import cats.parse.Rfc5234._
import cats.parse._
import worekleszczy.aoc.util.Files

import scala.annotation.tailrec

sealed trait Step extends Function2[String, String, String] {
  def apply(v1: String, v2: String): String =
    this match {
      case Right => v2
      case Left  => v1
    }
}
case object Right extends Step
case object Left  extends Step

object Puzzle extends App {

  def traverse(map: Map[String, (String, String)], start: String, stop: String)(steps: List[Step]): Int = {
    @tailrec
    def go(remaining: List[Step], current: String, hops: Int): Int = {
      val (left, right) = map(current)
      remaining match {
        case nextStep :: tail =>
          val nextNode = nextStep(left, right)
          if (nextNode == stop) hops else go(tail, nextNode, hops + 1)
        case Nil => go(steps, current, hops)
      }
    }
    go(steps, start, 1)
  }

  def traverseReapeat(map: Map[String, (String, String)], start: String, stop: String => Boolean)(
    steps: List[Step]
  ): List[(Int, String)] = {
    @tailrec
    def go(remaining: List[Step], current: String, endings: List[(Int, String)], hops: Int): List[(Int, String)] = {
      remaining match {
        case nextStep :: tail =>
          //          val nextNode = nextStep(left, right)
          //          if (nextNode == stop) hops else go(tail, nextNode, hops + 1)
          val pos = Function.tupled(nextStep)(map(current))
          go(tail, pos, endings, hops + 1)
        case Nil =>
          if (endings.exists { case (_, node) => node == current }) {
            endings
          } else {
            val withEndings = if (stop(current)) (hops - 1, current) :: endings else endings
            go(steps, current, withEndings, hops)
          }
      }
    }

    go(steps, start, Nil, 1)
  }

  @tailrec
  def greatestDenominator(a: Long, b: Long): Long = {
    if (a == 0) {
      b
    } else if (b == 0) {
      a
    } else {
      val q         = a / b
      val remainder = a - (q * b)
      greatestDenominator(b, remainder)
    }
  }

  def lowestCommonMultiplier(one: Long, two: Long): Long = {
    (one * two) / greatestDenominator(one, two)
  }

  val parseSteps = Parser.anyChar.map(c => if (c == 'R') Right else Left).rep
  val nodeParser = Parser.until(sp.orElse(Parser.char(',')).orElse(Parser.char(')')))
  val parseMapLine =
    (nodeParser <* sp.rep0.void <* Parser.char('=') <* sp.rep0) ~ ((nodeParser <* Parser.char(',') <* sp) ~ nodeParser)
      .surroundedBy(Parser.char('(').orElse(Parser.char(')')))

  val lines = Files.readLines("day8/input")
  val steps = parseSteps.parseAll(lines.head).toOption.get
  val nodeMap = lines.drop(2).flatMap(parseMapLine.parseAll(_).toOption).foldLeft(Map.empty[String, (String, String)]) {
    case (acc, (key, (left, right))) => acc.updated(key, (left, right))
  }

  val startNodes = nodeMap.keys.filter(_.endsWith("A")).toList

  println(s"Steps: ${steps.size}")
  println(startNodes
    .map { node =>
      traverseReapeat(nodeMap, node, _.endsWith("Z"))(steps.toList).head
    }.foldLeft(1L) {
      case (lcm, (hops, _)) => lowestCommonMultiplier(lcm.toLong, hops.toLong)
    })
}