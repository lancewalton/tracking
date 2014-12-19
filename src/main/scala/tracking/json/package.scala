package tracking

import org.joda.time.LocalDate
import argonaut.{CodecJson, DecodeResult}
import argonaut.Argonaut.{IntDecodeJson, IntEncodeJson, ListDecodeJson, ListEncodeJson, OptionDecodeJson, OptionEncodeJson, StringDecodeJson, StringEncodeJson, ToJsonIdentity, casecodec2, casecodec3, casecodec4}
import tracking.model.{Complete, Dependency, Epic, EpicComposition, EpicStatus, IdentifierAndTitle, InProgress, NotStarted, ProjectStatus}
import tracking.model.architecture.Node
import tracking.model.architecture.Location
import tracking.model.architecture.Relationship
import tracking.model.architecture.Architecture

package object json {
  implicit val identifierWithTitleCodec = casecodec2(IdentifierAndTitle.apply, IdentifierAndTitle.unapply)("id", "title")

  implicit val epicStatusCodec = CodecJson[EpicStatus](
    (_: EpicStatus) match {
      case NotStarted => "Not Started".asJson
      case InProgress => "In Progress".asJson
      case Complete => "Complete".asJson
    },
    c => c.focus.string match {
      case Some("Not Started") => DecodeResult.ok(NotStarted)
      case Some("In Progress") => DecodeResult.ok(InProgress)
      case Some("Complete") => DecodeResult.ok(Complete)
      case Some(s) => DecodeResult.fail(s"'$s' is not a valid value for an EpicStatus. Allowed values are: 'Not Started', 'In Progress' and 'Complete'", c.history)
      case _ => DecodeResult.fail("Problem decoding an EpicStatus. Allowed values are: 'Not Started', 'In Progress' and 'Complete'", c.history)
    }
  )
    
  implicit val epicCompositionCodec = casecodec3(EpicComposition.apply, EpicComposition.unapply)("completedStores", "unstartedStories", "storiesInProgress")

  implicit val dependencyCodec = casecodec2(Dependency.apply, Dependency.unapply)("projectId", "epicId")

  implicit val epicCodec = casecodec4(Epic.apply, Epic.unapply)("identifiers", "status", "composition", "dependencies")

  implicit val localDateCodec: CodecJson[LocalDate] = {
    casecodec3[Int, Int, Int, LocalDate](
      new LocalDate(_, _, _),
      (d: LocalDate) => Option((d.getYear, d.getMonthOfYear, d.getDayOfMonth)))("year", "month", "day")
  }

  implicit val projectStatusCodec: CodecJson[ProjectStatus] = casecodec2(ProjectStatus.apply, ProjectStatus.unapply)("date", "epics")
  
	implicit val locationCodec: CodecJson[Location] = casecodec2(Location.apply, Location.unapply)("x", "y")
  implicit val nodeCodec: CodecJson[Node] = casecodec3(Node.apply, Node.unapply)("id", "description", "location")
  implicit val relationshipCodec: CodecJson[Relationship] = casecodec2(Relationship.apply, Relationship.unapply)("from", "to")
  implicit val architectureCodec: CodecJson[Architecture] = casecodec2(Architecture.apply, Architecture.unapply)("nodes", "relationships")
}