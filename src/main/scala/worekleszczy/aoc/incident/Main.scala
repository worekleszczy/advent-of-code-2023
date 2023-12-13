package worekleszczy.aoc.incident

import io.circe.generic.auto._
import io.circe.parser.decode
import worekleszczy.aoc.util.Files

final case class Selection(outcomeId: String)
final case class Ticket(ticketId: String, accountId: String, selections: List[List[Selection]])

object Main extends App {

  def uniqueId(selections: List[List[Selection]]): String = selections.flatten.sortBy(_.outcomeId).mkString(",")
  val duplicates = Files
    .readLines("accounts_2.json")
    .flatMap(decode[Ticket](_).toOption)
    .foldLeft(Map.empty[String, Map[String, List[String]]]) {
      case (acc, entry) =>
        acc.updatedWith(entry.accountId) {
          case Some(v) =>
            Some(v.updatedWith(uniqueId(entry.selections)) {
              case Some(tail) => Some(entry.ticketId :: tail)
              case None       => Some(entry.ticketId :: Nil)
            })
          case None => Some(Map(uniqueId(entry.selections) -> (entry.ticketId :: Nil)))
        }
    }.toVector.flatMap {
      case (accountId, tickets) => tickets.values.filter(_.size >= 2).map(tickets => accountId -> tickets)
    }

  val affectedAccount = duplicates.map(_._1).distinct
//  println(affectedAccount.size)

//  println(affectedAccount.mkString("(\'", "\',\'", "\')"))
  duplicates.map{case (id, tickets) => s"$id, ${tickets.mkString(";")}"}.foreach(println)

}
