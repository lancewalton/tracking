package tracking.model

import org.joda.time.LocalDate
import scalaz.\/

case class Project(identifiers: IdentifierAndTitle, statuses: List[ProjectStatus]) {
  def findEpic(statusDate: LocalDate, id: EpicId): Option[\/[Epic, EpicWithStories]] =
    statuses
      .sorted.reverse
      .find(!_.date.isAfter(statusDate))
      .flatMap { _.findEpic(id) }
}