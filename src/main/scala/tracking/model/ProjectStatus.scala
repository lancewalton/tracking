package tracking.model

import org.joda.time.LocalDate
import scalaz.syntax.equal._
import scalaz.std.string._
import scalaz.{Order, \/, -\/, \/-}

case class ProjectStatus(date: LocalDate, epics: List[Epic]) {
  def findEpic(id: EpicId): Option[Epic] = epics.find(_.identifiers.id === id)
  def epicsWithStatus(status: EpicStatus) = epics.filter(_.status === status)
}

object ProjectStatus {
  implicit object DefaultOrdering extends Ordering[ProjectStatus] {
    def compare(x: ProjectStatus, y: ProjectStatus) = x.date.compareTo(y.date)
  }

  implicit val projectStatusOrdering: Order[ProjectStatus] = scalaz.Order.fromScalaOrdering(DefaultOrdering)
}
