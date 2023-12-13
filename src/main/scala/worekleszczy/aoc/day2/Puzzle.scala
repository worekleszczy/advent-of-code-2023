package worekleszczy.aoc.day2
import cats.parse.Parser
import cats.parse.Parser
import cats.parse.Numbers.digits
import cats.parse.Rfc5234._
import cats.syntax.functor._
import cats.syntax.flatMap._
import worekleszczy.aoc.util.Files

final case class Round(red: Int, green: Int, blue: Int)
final case class Game(id: Int, rounds: List[Round])
object Puzzle extends App {
  val gameHeader = (Parser.ignoreCase("game").surroundedBy(sp.rep0)) *> digits.map(_.toInt) <* sp.rep0

  val roundParser = {
    val round = (for {
      number <- digits.surroundedBy(sp.rep0)
      color  <- alpha.rep.string
      _      <- Parser.char(',').rep0
    } yield number.toInt -> color)
      .rep(1)
      .map(_.foldLeft(Round(0, 0, 0)) {
        case (acc, (num, color)) =>
          color match {
            case "red"   => acc.copy(red = num)
            case "green" => acc.copy(green = num)
            case "blue"  => acc.copy(blue = num)
            case _       => acc
          }
      })
    (round <* Parser.char(';').?).rep0
  }
  val lineParser = ((gameHeader <* Parser.char(':')) ~ roundParser).map { case (id, round) => Game(id, round) }

  def process(file: String, filer: Game => Option[Int]) =
    Files
      .readLines(file)
      .flatMap(lineParser.parseAll(_).toOption)
      .flatMap(filer)
      .sum

  val part1 = process(
    "day2/input",
    g => Option.when(g.rounds.forall(round => round.red <= 12 && round.green <= 13 && round.blue <= 14))(g.id)
  )

  val part2Func: Game => Option[Int] = game => {
    val minRound = game.rounds.fold(Round(0, 0, 0)) {
      case (min, current) =>
        Round(math.max(min.red, current.red), math.max(min.green, current.green), math.max(min.blue, current.blue))
    }

    Some(minRound.red * minRound.green * minRound.blue)
  }
  val part2 = process(
    "day2/input",
    part2Func
  )

  println(part1)
  println(part2)

}
