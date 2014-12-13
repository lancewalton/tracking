package tracking

import scalaz.ValidationNel
import org.joda.time.LocalDate
import argonaut.Argonaut.{ IntDecodeJson, IntEncodeJson, ListDecodeJson, ListEncodeJson, StringDecodeJson, StringEncodeJson, casecodec2, casecodec3, casecodec4, casecodec5 }
import argonaut.CodecJson
import tracking.model.{ Dependency, Epic, ProjectStatus }
import tracking.repository.RepositoryLoadError
import tracking.model.IdentifierAndTitle
import tracking.model.EpicComposition
import tracking.model.EpicStatus
import tracking.model.NotStarted
import tracking.model.InProgress
import tracking.model.Complete
import argonaut._, Argonaut._

package object repository {
  implicit def identifierWithTitleCodec = casecodec2(IdentifierAndTitle.apply, IdentifierAndTitle.unapply)("id", "title")

  implicit def epicStatusCodec = CodecJson[EpicStatus](
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
    
  implicit def epicCompositionCodec = casecodec3(EpicComposition.apply, EpicComposition.unapply)("completedStores", "unstartedStories", "storiesInProgress")

  implicit def epicCodec = casecodec3(Epic.apply, Epic.unapply)("identifiers", "status", "composition")

  implicit def localDateCodec: CodecJson[LocalDate] = {
    casecodec3[Int, Int, Int, LocalDate](
      new LocalDate(_, _, _),
      (d: LocalDate) => Option((d.getYear, d.getMonthOfYear, d.getDayOfMonth)))("year", "month", "day")
  }

  implicit def dependencyCodec = casecodec2(Dependency.apply, Dependency.unapply)("project", "epicId")

  implicit def projectStatusCodec: CodecJson[ProjectStatus] = casecodec3(ProjectStatus.apply, ProjectStatus.unapply)("date", "epics", "dependencies")

  type LoadedRepository[T] = ValidationNel[RepositoryLoadError, T]
}