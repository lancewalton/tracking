package tracking.model

import monocle.Lens
import scalaz.Show

case class Meta(identifiers: IdentifierAndTitle, function: String)

object Meta {
  val identifiersLens = Lens[Meta, Meta, IdentifierAndTitle, IdentifierAndTitle](_.identifiers, (m, identifiers) => m.copy(identifiers = identifiers))
  val functionLens = Lens[Meta, Meta, String, String](_.function, (m, function) => m.copy(function = function))
  
  val idLens = identifiersLens composeLens IdentifierAndTitle.idLens
  val titleLens = identifiersLens composeLens IdentifierAndTitle.titleLens
}