package worekleszczy.aoc.day10

import cats.syntax.option._
import enumeratum.values.{CharEnum, CharEnumEntry}
import worekleszczy.aoc.day10.Pipe.{EastWest, NorthSouth}
import worekleszczy.aoc.util.Files

import scala.annotation.tailrec

sealed abstract class Pipe(val value: Char) extends CharEnumEntry {
  def west(pipe: Pipe): Boolean  = false
  def east(pipe: Pipe): Boolean  = false
  def north(pipe: Pipe): Boolean = false
  def south(pipe: Pipe): Boolean = false
}

object Pipe extends CharEnum[Pipe] {
  sealed trait North extends Pipe {
    override def north(pipe: Pipe): Boolean =
      pipe match {
        case NorthSouth => true
        case SouthEast  => true
        case SouthWest  => true
        case _          => false

      }
  }
  sealed trait South extends Pipe {
    override def south(pipe: Pipe): Boolean =
      pipe match {
        case NorthSouth => true
        case NorthEast  => true
        case NorthWest  => true
        case _          => false
      }
  }

  sealed trait East extends Pipe {
    override def east(pipe: Pipe): Boolean =
      pipe match {
        case EastWest  => true
        case SouthWest => true
        case NorthWest => true
        case _         => false
      }
  }
  sealed trait West extends Pipe {
    override def west(pipe: Pipe): Boolean =
      pipe match {
        case EastWest  => true
        case SouthEast => true
        case NorthEast => true
        case _         => false
      }
  }
  case object NorthSouth extends Pipe('|') with North with South
  case object EastWest   extends Pipe('-') with East with West
  case object NorthEast  extends Pipe('L') with North with East
  case object NorthWest  extends Pipe('J') with North with West
  case object SouthWest  extends Pipe('7') with South with West
  case object SouthEast  extends Pipe('F') with South with East

  lazy val values: IndexedSeq[Pipe] = findValues
}

object Puzzle extends App {
  def traverse(map: Vector[String], startElement: Char) = {
    val width      = map.head.length
    val southEdge  = map.size - 1
    val flattenMap = map.mkString("")
    val start      = flattenMap.indexOf(startElement)

    def decodeLocation(flatLoc: Int): (Int, Int) = {
      val vertical   = flatLoc / width
      val horizontal = flatLoc - (vertical * width)
      (vertical, horizontal)
    }
    def encodeLocation(ver: Int, hor: Int): Int = (ver * width) + hor

    def north(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (ver - 1 >= 0) encodeLocation(ver - 1, hor).some else none
    }

    def south(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (ver + 1 <= southEdge) encodeLocation(ver + 1, hor).some else none
    }

    def east(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (hor + 1 < width) encodeLocation(ver, hor + 1).some else none
    }

    def west(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (hor - 1 >= 0) encodeLocation(ver, hor - 1).some else none
    }

    def connectNorth(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- north(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.north(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    def connectSouth(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- south(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.south(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    def connectEast(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- east(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.east(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    def connectWest(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- west(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.west(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    @tailrec
    def loop(currentPipe: Pipe, currentPos: Int, previousPos: Int, steps: Int): Int = {
      val available = List(
        connectNorth(currentPipe, currentPos),
        connectSouth(currentPipe, currentPos),
        connectEast(currentPipe, currentPos),
        connectWest(currentPipe, currentPos)
      ).flatten
//      println(s"Current: $currentPipe, available: ${available}")

      available.find { case (_, index) => index != previousPos } match {
        case Some((nextPipe, nextPost)) => loop(nextPipe, nextPost, currentPos, steps + 1)
        case None                       => steps
      }
    }

    val result = loop(EastWest, start, start, 0)
    if (result == 0) loop(NorthSouth, start, start, 0) else result

  }

  def traverseTrail(map: Vector[String], startElement: Char) = {
    val width      = map.head.length
    val southEdge  = map.size - 1
    val flattenMap = map.mkString("")
    val start      = flattenMap.indexOf(startElement)

    def decodeLocation(flatLoc: Int): (Int, Int) = {
      val vertical   = flatLoc / width
      val horizontal = flatLoc - (vertical * width)
      (vertical, horizontal)
    }

    def encodeLocation(ver: Int, hor: Int): Int = (ver * width) + hor

    def north(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (ver - 1 >= 0) encodeLocation(ver - 1, hor).some else none
    }

    def south(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (ver + 1 <= southEdge) encodeLocation(ver + 1, hor).some else none
    }

    def east(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (hor + 1 < width) encodeLocation(ver, hor + 1).some else none
    }

    def west(loc: Int): Option[Int] = {
      val (ver, hor) = decodeLocation(loc)
      if (hor - 1 >= 0) encodeLocation(ver, hor - 1).some else none
    }

    def connectNorth(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- north(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.north(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    def connectSouth(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- south(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.south(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    def connectEast(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- east(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.east(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    def connectWest(pipe: Pipe, loc: Int): Option[(Pipe, Int)] = {
      for {
        nextLoc  <- west(loc)
        nextPipe <- Pipe.withValueOpt(flattenMap.charAt(nextLoc))
        result   <- if (pipe.west(nextPipe)) (nextPipe, nextLoc).some else none
      } yield result
    }

    @tailrec
    def loop(currentPipe: Pipe, currentPos: Int, previousPos: Int, positions: List[Int]): List[Int] = {
      val available = List(
        connectNorth(currentPipe, currentPos),
        connectSouth(currentPipe, currentPos),
        connectEast(currentPipe, currentPos),
        connectWest(currentPipe, currentPos)
      ).flatten

      available.find { case (_, index) => index != previousPos } match {
        case Some((nextPipe, nextPost)) => loop(nextPipe, nextPost, currentPos, currentPos :: positions)
        case None                       => currentPos :: positions
      }
    }

    val maybeResult = loop(EastWest, start, start, Nil)
    val result =
      (if (maybeResult.size <= 1 ) loop(NorthSouth, start, start, Nil) else maybeResult).map(decodeLocation)
//    val trail = (result ::: result.head :: Nil).foldLeft(List.empty[(Int, Int)]) {
    val trail = (decodeLocation(start) :: result ).foldLeft(List.empty[(Int, Int)]) {
      case (acc @ ((bx, by) :: (a @ (ax, ay)) :: rest), c @ (cx, cy)) =>
        val sameHorizontal = bx == ax && ax == cx
        val sameVertical   = by == ay && ay == cy

        if (sameHorizontal || sameVertical) c :: a :: rest else c :: acc
      case (r, next) => next :: r
    }
    println(result)
    println(trail)
    val intersectFunctions = (trail ::: trail.head :: Nil)
      .sliding(2)
      .collect {
        case ((x1, y1) :: (x2, y2) :: Nil)  if x1 != x2 || y1 != y2=>
          println(s"Segment: $x1,$y1, $x2,$y2")
          intersect(x1, y1, x2, y2)
      }
      .toList

    val inside = (0 until flattenMap.length).map(decodeLocation).filter {
      case (x, y) =>
        !result.contains((x, y)) && intersectFunctions.count(_.apply(x, y)) % 2 == 1
    }

    inside.toList
  }
  def intersect(x1: Int, y1: Int, x2: Int, y2: Int): (Int, Int) => Boolean =
    (x, y) => {
      x <= math.max(x1, x2) && x > math.min(x1, x2) && y1 >= y
    }

  val startElement = 'S'
  val map          = Files.readLines("day10/input").toVector

  println(traverseTrail(map, startElement).size)
}
