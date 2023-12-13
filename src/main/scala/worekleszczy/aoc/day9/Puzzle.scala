package worekleszczy.aoc.day9

import cats.parse.Numbers._
import cats.parse.Rfc5234._
import worekleszczy.aoc.util.Files

object Puzzle extends App {

  val lineParser = (signedIntString.map(_.toLong) <* sp.rep0).rep

  def extrapolate(signal: List[Long]): Long = {
    signal
      .foldLeft(List.empty[Long]) {
        case (acc, signalValues) => acc.scanLeft(signalValues) { case (sub, base) => sub - base }
      }
      .foldRight(0L)(_ + _)
  }

  def extrapolateBackwards(signal: List[Long]): Long = {
    signal.reverse
      .foldLeft(List.empty[Long]) {
        case (acc, signalValues) => acc.scanLeft(signalValues) { case (sub, base) => base - sub }
      }
      .foldRight(0L)(_ - _)
  }
  def processInput(extrapolate: List[Long] => Long): Long =
    Files
      .readLines("day9/input")
      .flatMap(lineParser.parseAll(_).toOption)
      .map(x => extrapolate(x.toList))
      .sum
  def partOne = processInput(extrapolate)
  def partTwo = processInput(extrapolateBackwards)


  println(partOne)
  println(partTwo)
}
