package worekleszczy.aoc.day4
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Numbers._
import cats.parse.Rfc5234._
import worekleszczy.aoc.util.Files

final case class State(won: Int, next: List[Int])
object Puzzle extends App {

  val header = Parser.ignoreCase("card") *> sp.rep.void *> digits.map(_.toInt) <* Parser.char(':')
  val number = sp.rep0.void.with1 *> digits.map(_.toInt)
  val lineParser = ((header ~ number.repUntil(sp.rep0 *> Parser.char('|'))) <* (sp.rep0 *> Parser.char(
    '|'
  ))) ~ number.rep //<* (sp.rep0 *> Parser.char('|'))) ~ numbersList

  def calculatePoints(winning: NonEmptyList[Int], players: NonEmptyList[Int]): Int = {
    val winningNumbers = players.filter(winning.toList.contains).size
    if (winningNumbers == 0) 0 else math.pow(2.0, winningNumbers - 1).toInt
  }

  def copiesCards(winning: NonEmptyList[Int], players: NonEmptyList[Int]): Int = {
    players.filter(winning.toList.contains).size
  }

  val result1 = Files
    .readLines("day4/input")
    .flatMap(lineParser.parseAll(_).toOption)
    .map { case ((_, winning), current) => calculatePoints(winning, current) }
    .sum

  val scan = Files
    .readLines("day4/input")
    .flatMap(lineParser.parseAll(_).toOption)
    .foldLeft(State(0, Nil)) {
      case (state, ((card, wonNumbers), current)) =>
        val wonCards  = copiesCards(wonNumbers, current)
        val addCopies = List.fill(wonCards)(1)

        state.next match {
          case head :: tail =>
            state.copy(
              won = head + 1 + state.won,
              next = tail.zipAll(addCopies.map(x => x * (head + 1)), 0, 0).map { case (x, y) => x + y }
            )
          case Nil =>
            state.copy(won = 1 + state.won, next = addCopies)
        }

    }



}
