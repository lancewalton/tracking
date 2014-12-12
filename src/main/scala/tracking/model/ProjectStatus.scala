package tracking.model

import org.joda.time.LocalDate
import scalaz.syntax.equal._
import scalaz.std.string._
import scalaz.{\/, -\/, \/-}

case class ProjectStatus(date: LocalDate, completedEpics: List[Epic], unstartedEpics: List[Epic], epicsInProgress: List[EpicWithStories], dependencies: List[Dependency]) {
  def epicsInProject = completedEpics ::: unstartedEpics ::: epicsInProgress.map(_.epic)
  
  def findEpic(id: EpicId): Option[\/[Epic, EpicWithStories]] =
    (completedEpics ::: unstartedEpics).find(_.id === id).map(-\/(_)) orElse epicsInProgress.find(_.epic.id === id).map(\/-(_))
}

object ProjectStatus {
  implicit object DefaultOrdering extends Ordering[ProjectStatus] {
    def compare(x: ProjectStatus, y: ProjectStatus) = x.date.compareTo(y.date)
  }
}