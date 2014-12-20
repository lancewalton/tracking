package tracking.model

import scalaz.Show
import monocle.Lens

case class IdentifierAndTitle(id: String, title: String)

object IdentifierAndTitle {
  val idLens = Lens[IdentifierAndTitle, IdentifierAndTitle, String, String](_.id, (m, id) => m.copy(id = id))
  val titleLens = Lens[IdentifierAndTitle, IdentifierAndTitle, String, String](_.title, (m, title) => m.copy(title = title))

  implicit val show = new Show[IdentifierAndTitle] {
    override def shows(p: IdentifierAndTitle) = s"(title: '${p.title}', id: '${p.id}')"
  }
}