package worekleszczy.aoc.day1

import worekleszczy.aoc.util.Files

import scala.util.matching.Regex.Match

object Puzzle extends App {
  def processLinePartOne(line: String): Option[(Int, Int)] = {
    for {
      first <- line.find(_.isDigit)
      last  <- line.findLast(_.isDigit)
    } yield (first.asDigit, last.asDigit)
  }

  val mapping = Map(
    "one"   -> 1,
    "two"   -> 2,
    "three" -> 3,
    "four"  -> 4,
    "five"  -> 5,
    "six"   -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine"  -> 9
  )
  val regex = "(?=(one|two|three|four|five|six|seven|eight|nine|\\d))".r

  def matchToInt(m: Match): Option[Int] = {
    val found = m.group(1)
    mapping.get(found).orElse(found.toIntOption)
  }

  def processLinePartTwo(line: String): Option[(Int, Int)] = {
    val matches = regex.findAllMatchIn(line).toList.sortBy(_.start)
    for {
      first    <- matches.headOption
      firstNum <- matchToInt(first)
      last     <- matches.lastOption
      lastNum  <- matchToInt(last)
    } yield (firstNum, lastNum)
  }

  def processFile(filename: String, processFn: String => Option[(Int, Int)]): Int = {
    Files
      .readLines(filename)
      .map(processFn(_).get) // TODO unsafe
      .map { case (first, last) => first * 10 + last }
      .sum
  }

  println(s"Part one: ${processFile("input", processLinePartOne)}")
  println(s"Part two: ${processFile("input", processLinePartTwo)}")
}
