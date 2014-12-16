package tracking.model

import org.joda.time.LocalDate
import scalaz.\/

case class Project(identifiers: IdentifierAndTitle, statuses: List[ProjectStatus]) {
  def findEpic(statusDate: LocalDate, id: EpicId): Option[Epic] = {
    for {
      s ← status(statusDate)
      e ← s.findEpic(id)
    } yield e
  }
    
  def status(date: LocalDate) = statuses.sorted.reverse.find(!_.date.isAfter(date))
}