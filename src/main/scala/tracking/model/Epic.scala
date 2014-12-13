package tracking.model

case class Epic(identifiers: IdentifierAndTitle, status: EpicStatus, composition: Option[EpicComposition])
