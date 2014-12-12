package tracking.model

import scalaz.Show

case class Dependency(projectId: ProjectId, epicId: EpicId)

object Dependency {
  implicit val show = new Show[Dependency] {
    override def shows(p: Dependency) = s"(projectId: '${p.projectId}', epicId: '${p.epicId}')"
  }
}