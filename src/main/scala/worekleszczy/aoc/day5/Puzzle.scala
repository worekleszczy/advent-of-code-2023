package worekleszczy.aoc.day5

import cats.parse.Numbers._
import cats.parse.Parser
import cats.parse.Rfc5234._
import worekleszczy.aoc.util.Files
import scala.concurrent.duration._

final case class Range(sourceStart: Long, destinationStart: Long, length: Long) {
  private val diff = sourceStart - destinationStart
  def mapOption(source: Long): Option[Long] = {
    if (source >= sourceStart && source < sourceStart + length) {
      Some(source - diff)
    } else None
  }

  def contramapOption(value: Long): Option[Long] = {
    if (value >= destinationStart && value < destinationStart + length) {
      Some(value + diff)
    } else None
  }
}

final case class SeedRange(start: Long, length: Long) {
  def within(value: Long): Boolean = value >= start && value < start + length
}

final case class RangeMap(ranges: List[Range]) {
  private def filterFunction(source: Long): PartialFunction[Range, Long] = Function.unlift(_.mapOption(source))
  def map(source: Long): Long                                            = ranges.collectFirst(filterFunction(source)).getOrElse(source)
  def contraMap(value: Long): Long                                       = ranges.collectFirst(Function.unlift(_.contramapOption(value))).getOrElse(value)
}

object RangeMap {
  def fromRanges(range: List[(Long, Long, Long)]): RangeMap = {
    val ranges = range.map {
      case (dest, source, length) => Range(source, dest, length)
    }
    RangeMap(ranges)
  }
}

object Puzzle extends App {

  val file = Files
    .readLines("day5/input")
    .mkString("\n")

  val emptyLine = "(?m)^\n".r

  val dropHeader  = Parser.charsWhile(_ != ':').void *> Parser.char(':') *> (sp.orElse(Parser.char('\n')))
  val numbersLine = (sp.rep0.with1 *> digits.map(_.toLong)).rep <* Parser.char('\n').orElse(Parser.end)

  val parsePart = dropHeader *> numbersLine.rep

  val parts = file.split("(?m)^\n").toVector.flatMap(parsePart.parseAll(_).toOption)

  val seeds = parts(0).head

  val seedsToSoilMap = RangeMap.fromRanges(parts(1).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))
  val soilToFertilizer = RangeMap.fromRanges(parts(2).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))
  val fertilizerToWater = RangeMap.fromRanges(parts(3).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))
  val waterToLight = RangeMap.fromRanges(parts(4).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))
  val lightToTemp = RangeMap.fromRanges(parts(5).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))
  val tempToHumidity = RangeMap.fromRanges(parts(6).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))
  val humidityToLocation = RangeMap.fromRanges(parts(7).collect(_.toList match {
    case x :: y :: z :: Nil => (x, y, z)
  }))

//  println(
//    seeds
//      .map { seed =>
//        (seedsToSoilMap.map _)
//          .andThen(soilToFertilizer.map)
//          .andThen(fertilizerToWater.map)
//          .andThen(waterToLight.map)
//          .andThen(lightToTemp.map)
//          .andThen(tempToHumidity.map)
//          .andThen(humidityToLocation.map)
//          .apply(seed)
//      }
//      .toList
//      .min
//  )

//  println(
//    LazyList
//      .from(
//        seeds.toList
//          .grouped(2)
//      )
//      .flatMap { group =>
//        println("Generating a group")
//        val start  = group(0)
//        val length = group(1)
//        LazyList.range(start, start + length)
//      }
//      .map { seed =>
//        (seedsToSoilMap.map _)
//          .andThen(soilToFertilizer.map)
//          .andThen(fertilizerToWater.map)
//          .andThen(waterToLight.map)
//          .andThen(lightToTemp.map)
//          .andThen(tempToHumidity.map)
//          .andThen(humidityToLocation.map)
//          .apply(seed)
//      }
//      .min
//  )
//  (0 until 100).foreach { x => println(s"$x ${seedsToSoilMap.contraMap(x)}")}

  val seedRanges = seeds.toList
    .grouped(2)
    .map { group =>
      val start  = group(0)
      val length = group(1)
      SeedRange(start, length)
    }
    .toList

  val before = System.nanoTime()
  println(
    LazyList
      .range(0, 1000000000L)
      .map { loc =>
        val seed = (humidityToLocation.contraMap _)
          .andThen(tempToHumidity.contraMap)
          .andThen(lightToTemp.contraMap)
          .andThen(waterToLight.contraMap)
          .andThen(fertilizerToWater.contraMap)
          .andThen(soilToFertilizer.contraMap)
          .andThen(seedsToSoilMap.contraMap)
          .apply(loc)
        (loc, seed)
      }
      .filter { case (_, seed) => seedRanges.exists(_.within(seed)) }
      .map { case (loc, _) => loc }
      .head
  )
  val after = System.nanoTime()
  println((after - before).nano.toMillis)

}
