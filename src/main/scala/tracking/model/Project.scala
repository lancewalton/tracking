package tracking.model

import org.joda.time.LocalDate
import scalaz.\/

case class Project(identifiers: IdentifierAndTitle, statuses: List[ProjectStatus]) {
  def findEpic(statusDate: LocalDate, id: EpicId): Option[Epic] =
    for {
      s ← statuses.sorted.reverse.find(!_.date.isAfter(statusDate))
      e ← s.findEpic(id)
    } yield e
}