package tracking.model

import org.joda.time.LocalDate
import scalaz.\/
import monocle.Lens

case class Project(meta: Meta, statuses: List[ProjectStatus]) {
  def findEpic(statusDate: LocalDate, id: EpicId): Option[Epic] = {
    for {
      s ← status(statusDate)
      e ← s.findEpic(id)
    } yield e
  }
    
  def status(date: LocalDate) = statuses.sorted.reverse.find(!_.date.isAfter(date))
}

object Project {
  val metaLens = Lens[Project, Project, Meta, Meta](_.meta, (p, m) => p.copy(meta = m))
  val identifiersLens = metaLens composeLens Meta.identifiersLens
  val idLens = metaLens composeLens Meta.idLens
  val titleLens = metaLens composeLens Meta.titleLens
}