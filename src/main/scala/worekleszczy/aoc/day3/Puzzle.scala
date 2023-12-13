package worekleszczy.aoc.day3
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.parse.Numbers.digits
import worekleszczy.aoc.util.Files
import cats.syntax.option._

final case class EngineNumber(index: Int, length: Int, value: Int) {
  def adjacentTo(symbolIndex: Int): Boolean = {
    val min = index - 1
    val max = index + length
    if (symbolIndex >= min && symbolIndex <= max) true else false

  }

}
case class Line(symbols: List[Int], engineNumbers: List[EngineNumber], gears: List[Int]) {
  def adjacent(prev: Line, next: Line): List[Int] = {
    val allSymbolsIndex = symbols ::: prev.symbols ::: next.symbols
    engineNumbers
      .filter { num =>
        allSymbolsIndex.exists(num.adjacentTo)
      }
      .map(_.value)
  }

  def adjacentGears(prev: Line, next: Line): List[(Int, Int)] = {
    val allNumbers = prev.engineNumbers ::: engineNumbers ::: next.engineNumbers
    gears.flatMap { index =>
      val nums = allNumbers.filter(_.adjacentTo(index))
      if (nums.size == 2) (nums.head.value, nums(1).value).some else none
    }
  }

}

object Puzzle extends App {

  val values = (Parser.index.with1 ~ digits)
    .eitherOr((Parser.index.with1 ~ Parser.anyChar))

  val lineParser = Parser
    .char('.')
    .rep
    .void
    .eitherOr(values)
    .rep
    .map(_.foldLeft(Line(Nil, Nil, Nil)) {
      case (line, Left(Right((index, value)))) =>
        line.copy(engineNumbers = EngineNumber(index, value.length, value.toInt) :: line.engineNumbers)
      case (line, Left(Left((index, value)))) =>
        val gears = if (value == '*') index :: line.gears else line.gears
        line.copy(symbols = index :: line.symbols, gears = gears)
      case (line, _) => line
    })

  val lines = Files
    .readLines("day3/input")
    .flatMap(lineParser.parseAll(_).toOption)

  val result = (Line(Nil, Nil, Nil) :: lines ::: List(Line(Nil, Nil, Nil)))
    .sliding(3)
    .flatMap {
      case prev :: current :: next :: Nil => current.adjacent(prev, next).sum.some
      case _                              => none
    }
    .sum
  val result2 = (Line(Nil, Nil, Nil) :: lines ::: List(Line(Nil, Nil, Nil)))
    .sliding(3)
    .flatMap {
      case prev :: current :: next :: Nil => current.adjacentGears(prev, next).map { case (x, y) => x * y }.sum.some
      case _                              => none
    }
    .sum

  println(result2)

}
