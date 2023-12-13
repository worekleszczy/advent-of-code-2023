package worekleszczy.aoc.day12

import enumeratum.values.{CharEnum, CharEnumEntry}
import worekleszczy.aoc.util.Files

sealed abstract class Symbol(val value: Char) extends CharEnumEntry
object Symbol extends CharEnum[Symbol] {

  final case object Broken      extends Symbol('#')
  final case object Unknown     extends Symbol('?')
  final case object Operational extends Symbol('.')

  lazy val values: IndexedSeq[Symbol] = findValues
}

object Puzzle extends App {

  def fit(undefined: Int, broken: List[Int]): Option[Int] = None

//  def combinations(brokenFirst: Boolean, anchored: Boolean, segments: List[Int], broken: List[Int]): Int = {
//    (segments, broken) match {
//      case (segment :: tail, first :: rest) if brokenFirst =>
//        if (segment > first) {
//          0
//        } else if (segment == first) {
//          combinations(!brokenFirst, false, tail, rest)
//        } else {
//          combinations(!brokenFirst, true, tail, (first - segment) :: rest)
//        }
//      case (segment :: tail, first :: rest) if !brokenFirst =>
//        lazy val f = if (!anchored) {
//          val modSegments = if (segment - 1 > 0) {
//            (segment - 1) :: tail
//          } else tail
//          combinations(
//            false,
//            false,
//            modSegments,
//            broken
//          )
//        } else 0
//
//        if (segment >= (first + 1)) {
//          combinations(false, false, (segment - (first + 1)) :: tail, rest) + f
//        } else {
//          if (first - segment == 0) {
//            if (rest.isEmpty) {
//              1
//            } else f
//          } else {
//            combinations(true, false, tail, (first - segment) :: rest) + f
//          }
//        }
//      case (_, Nil) => 1
//      case (_, _)   => 0
//    }
//
//  }

  def combinations(brokenFirst: Boolean, anchored: Boolean, segments: List[Int], broken: List[Int]): Int = {
    (segments, broken) match {
      case (segment :: tail, first :: rest) if brokenFirst =>
        if (segment > first) {
          0
        } else if (segment == first) {
          //we need to insert a space here
          combinations(!brokenFirst, false, tail, rest)
        } else {
          combinations(!brokenFirst, true, tail, (first - segment) :: rest)
        }
      case (segment :: tail, first :: rest) if !brokenFirst =>
        lazy val f = if (!anchored) {
          val modSegments = if (segment - 1 > 0) {
            (segment - 1) :: tail
          } else tail
          combinations(
            false,
            false,
            modSegments,
            broken
          )
        } else 0

        if (segment >= (first + 1)) {
          combinations(false, false, (segment - (first + 1)) :: tail, rest) + f
        } else {
          if (first - segment == 0) {
            if (rest.isEmpty) {
              1
            } else f
          } else {
            combinations(true, false, tail, (first - segment) :: rest) + f
          }
        }
      case (_, Nil) => 1
      case (_, _)   => 0
    }

  }

  def complete(segments: List[(Symbol, Int)], expected: List[Int]): List[String] = {
    def loop(
      needSpace: Boolean,
      anchored: Boolean,
      segments: List[(Symbol, Int)],
      expected: List[Int],
      result: List[Symbol]
    ): List[List[Symbol]] = {
      expected match {
        case nextExpected :: remainingExpected =>
          segments match {
            case (Symbol.Operational, l) :: remainingSegments if !anchored =>
              loop(false, false, remainingSegments, expected, List.fill(l)(Symbol.Operational) ::: result)
            case (Symbol.Operational, _) :: _ =>
              Nil
            case (Symbol.Broken, segmentLength) :: remainingSegments =>
              if (needSpace || segmentLength > nextExpected) {
                Nil
              } else if (segmentLength == nextExpected) {
                loop(
                  true,
                  false,
                  remainingSegments,
                  remainingExpected,
                  List.fill(segmentLength)(Symbol.Broken) ::: result
                )
              } else {
                loop(
                  false,
                  true,
                  remainingSegments,
                  nextExpected - segmentLength :: remainingExpected,
                  List.fill(segmentLength)(Symbol.Broken) ::: result
                )
              }
            case (Symbol.Unknown, segmentLength) :: remainingSegments =>
              if (needSpace) {
                val remaining =
                  if (segmentLength > 1) (Symbol.Unknown, segmentLength - 1) :: remainingSegments else remainingSegments
                loop(false, false, remaining, expected, Symbol.Operational :: result)
              } else {
                lazy val following = if (!anchored) {
                  val computedSegments =
                    if (segmentLength > 1) (Symbol.Unknown, segmentLength - 1) :: remainingSegments
                    else remainingSegments

                  loop(false, false, computedSegments, expected, Symbol.Operational :: result)
                } else Nil

                if (segmentLength > nextExpected) {
                  loop(
                    true,
                    false,
                    (Symbol.Unknown, segmentLength - nextExpected) :: remainingSegments,
                    remainingExpected,
                    List.fill(nextExpected)(Symbol.Broken) ::: result
                  ) ::: following
                } else if (segmentLength == nextExpected) {
                  loop(
                    true,
                    false,
                    remainingSegments,
                    remainingExpected,
                    List.fill(nextExpected)(Symbol.Broken) ::: result
                  ) ::: following
                } else {
                  loop(
                    false,
                    true,
                    remainingSegments,
                    (nextExpected - segmentLength) :: remainingExpected,
                    List.fill(segmentLength)(Symbol.Broken) ::: result
                  ) ::: following
                }
              }
            case Nil => Nil
          }
        case Nil =>
          if (segments.exists { case (s, _) => s == Symbol.Broken }) {
            Nil
          } else result.reverse :: Nil
      }
    }

    loop(false, false, segments, expected, Nil).map(_.map(_.value).mkString("")).distinct
  }

  def parse(line: String): Option[(List[(Symbol, Int)], List[Int])] = {
    line.split(" ").toList match {
      case spring :: broken :: Nil =>
        val expected = broken.split(",").toList.map(_.toInt)
        Some(
          spring
            .foldLeft(List.empty[(Symbol, Int)]) {
              case (acc, current) =>
                val s = Symbol.withValue(current)
                acc.headOption.filter(_._1 == s).map { case (s, i) => (s, i + 1) }.fold((s, 1) :: acc)(_ :: acc.tail)
            }
            .reverse -> expected
        )
      case _ => None
    }
  }

//  println("?????????????#???.?? 4,8,1")
//  println(parse("??.???# 1,3").map(Function.tupled(complete)).foreach(_.foreach(println)))

  val result = Files
    .readLines("day12/input")
    .flatMap(parse)
    .map {case (springs, checksum) =>
      val rep = springs ::: (Symbol.Unknown, 1) :: Nil
      (rep ::: rep ::: rep ::: rep ::: springs, checksum ::: checksum ::: checksum ::: checksum ::: checksum)
    }
    .zipWithIndex
    .map { case ((springs, check), index) =>

      val res = complete(springs, check).size
      println(s"Computed $index")
     res
    }
    .sum

  println(result)
}
