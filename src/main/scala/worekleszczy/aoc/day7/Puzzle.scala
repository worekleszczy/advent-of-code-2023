package worekleszczy.aoc.day7

import cats.syntax.option._
import worekleszczy.aoc.util.Files
final case class Card(value: Char) extends AnyVal
object Card {
  private val symbolOrderMap = Map(
    'A' -> 13,
    'K' -> 12,
    'Q' -> 11,
    'J' -> 0,
//    'J' -> 10,
    'T' -> 9,
    '9' -> 8,
    '8' -> 7,
    '7' -> 6,
    '6' -> 5,
    '5' -> 4,
    '4' -> 3,
    '3' -> 2,
    '2' -> 1
  )

  def fromChar(value: Char): Option[Card] = symbolOrderMap.get(value).map(_ => Card(value))

  implicit val order: Ordering[Card] = Ordering[Int].on[Card](c => symbolOrderMap(c.value))
}

sealed abstract class Figure(val power: Int) {}

object Figure {
  case object Five     extends Figure(7)
  case object Four     extends Figure(6)
  case object Full     extends Figure(5)
  case object Three    extends Figure(4)
  case object TwoPair  extends Figure(3)
  case object Two      extends Figure(2)
  case object HighCard extends Figure(1)

  private val repeating = "(?=(.))(\\1)+".r
  def parse(value: String): Option[Figure] = {
    repeating.findAllMatchIn(value.sorted).map(_.group(0).length).toList.sorted(Ordering[Int].reverse) match {
      case x :: _ if x == 5                => Five.some
      case x :: _ if x == 4                => Four.some
      case x :: y :: _ if x == 3 && y == 2 => Full.some
      case x :: _ if x == 3                => Three.some
      case x :: y :: _ if x == 2 && y == 2 => TwoPair.some
      case x :: _ if x == 2                => Two.some
      case x :: _ if x == 1                => HighCard.some
      case _                               => None
    }
  }

  def parseJokers(value: String): Option[Figure] = {
    val jokers = value.count(_ == 'J')
    repeating
      .findAllMatchIn(value.replace("J", "").sorted)
      .map(_.group(0).length)
      .toList
      .sorted(Ordering[Int].reverse) match {
      case x :: _ if x + jokers == 5                => Five.some
      case x :: _ if x + jokers == 4                => Four.some
      case x :: y :: _ if x + jokers == 3 && y == 2 => Full.some
      case x :: _ if x + jokers == 3                => Three.some
      case x :: y :: _ if x == 2 && y == 2          => TwoPair.some
      case x :: _ if x + jokers == 2                => Two.some
      case _ if jokers == 5                         => Five.some
      case _                                        => HighCard.some
    }
  }

  implicit val oderding: Ordering[Figure] = Ordering.by(_.power)
}

final case class Hand(figure: Figure, one: Card, two: Card, three: Card, four: Card, five: Card)

object Hand {
  implicit val ordering: Ordering[Hand] = Ordering.by { hand =>
    (hand.figure, hand.one, hand.two, hand.three, hand.four, hand.five)
  }

  def parse(value: String): Option[Hand] = {
    value.flatMap(Card.fromChar).toList match {
      case one :: two :: three :: four :: five :: Nil =>
        Figure.parse(value).map(f => Hand(f, one, two, three, four, five))
      case Nil => None
    }
  }

  def parseJokers(value: String): Option[Hand] = {
    value.flatMap(Card.fromChar).toList match {
      case one :: two :: three :: four :: five :: Nil =>
        Figure.parseJokers(value).map(f => Hand(f, one, two, three, four, five))
      case Nil => None
    }
  }
}

object Puzzle extends App {

  val result = Files
    .readLines("day7/input")
    .flatMap(_.split(" ").toList match {
      case hand :: bid :: Nil => Hand.parseJokers(hand).map(_ -> bid.toLong)
      case _                  => none
    })
//    .foreach(println)
    .sortBy(_._1)
    .zipWithIndex
    .map {
      case ((_, bid), index) => bid * (index + 1)
    }
    .sum
//
//248889565
  //250267306 to high
  // 249390065 wrong
  println(result)
}
