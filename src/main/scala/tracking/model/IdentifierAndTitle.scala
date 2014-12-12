package tracking.model

import scalaz.Show

case class IdentifierAndTitle(id: String, title: String)

object IdentifierAndTitle {
  implicit val show = new Show[IdentifierAndTitle] {
    override def shows(p: IdentifierAndTitle) = s"(title: '${p.title}', id: '${p.id}')"
  }
}