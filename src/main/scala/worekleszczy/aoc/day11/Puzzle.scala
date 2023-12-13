package worekleszczy.aoc.day11

import worekleszczy.aoc.util.Files

import scala.annotation.tailrec
import scala.util.chaining._

object Puzzle extends App {

  val input = Files.readLines("day11/input")

  val width = input.head.length

  def decodeLocation(flatLoc: Int): (Int, Int) = {
    val vertical   = flatLoc / width
    val horizontal = flatLoc - (vertical * width)
    (vertical, horizontal)
  }
  def encodeLocation(ver: Int, hor: Int): Int = (ver * width) + hor

  def galaxiesLocations(lines: List[String]): List[(Int, Int)] = {
    val flatInput = lines.mkString("")
    @tailrec
    def loop(loc: Int, galaxies: List[(Int, Int)]): List[(Int, Int)] = {
      val index = flatInput.indexOf('#', loc)

      if (index < 0) galaxies else loop(index + 1, decodeLocation(index) :: galaxies)
    }

    loop(0, Nil)
  }

  val galaxies = galaxiesLocations(input)

  val (rows, cols) = galaxies.unzip.pipe {
    case (rows, cols) =>
      (rows.toSet, cols.toSet)
  }

  def distance(first: (Int, Int), second: (Int, Int)): Int = {
    val (firstRow, firstCol)   = first
    val (secondRow, secondCol) = second

    val rowDistance =
      (math.min(firstRow, secondRow) until math.max(firstRow, secondRow)).map { row =>
        if (rows.contains(row + 1)) 1 else 2
      }.sum
    val colDistance =
      (math.min(firstCol, secondCol) until math.max(firstCol, secondCol))
        .map(col => if (cols.contains(col + 1)) 1 else 2)
        .sum

    rowDistance + colDistance
  }

  def distanceTwo(first: (Int, Int), second: (Int, Int)): Long = {
    val (firstRow, firstCol) = first
    val (secondRow, secondCol) = second

    val rowDistance =
      (math.min(firstRow, secondRow) until math.max(firstRow, secondRow)).map { row =>
        if (rows.contains(row + 1)) 1L else 1000000L
      }.sum
    val colDistance =
      (math.min(firstCol, secondCol) until math.max(firstCol, secondCol))
        .map(col => if (cols.contains(col + 1)) 1L else 1000000L)
        .sum

    rowDistance + colDistance
  }

  println(galaxies.map { galaxy =>
    galaxies
      .dropWhile(_ != galaxy)
      .tail
      .map(g2 => distanceTwo(galaxy, g2))
      .sum
  }.sum)
}
