package worekleszczy.aoc.util

import scala.io.Source

object Files {

  def readLines(name: String): List[String] = Source.fromResource(name).getLines().toList
}
