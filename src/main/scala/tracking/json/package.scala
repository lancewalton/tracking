package tracking

import org.joda.time.LocalDate

import argonaut.{CodecJson, DecodeResult}
import argonaut.Argonaut.{IntDecodeJson, IntEncodeJson, ListDecodeJson, ListEncodeJson, OptionDecodeJson, OptionEncodeJson, StringDecodeJson, StringEncodeJson, ToJsonIdentity, casecodec2, casecodec3, casecodec4}
import tracking.model.{Complete, Dependency, Epic, EpicComposition, EpicStatus, IdentifierAndTitle, InProgress, NotStarted, ProjectStatus}

package object json {
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

  implicit def dependencyCodec = casecodec2(Dependency.apply, Dependency.unapply)("projectId", "epicId")

  implicit def epicCodec = casecodec4(Epic.apply, Epic.unapply)("identifiers", "status", "composition", "dependencies")

  implicit def localDateCodec: CodecJson[LocalDate] = {
    casecodec3[Int, Int, Int, LocalDate](
      new LocalDate(_, _, _),
      (d: LocalDate) => Option((d.getYear, d.getMonthOfYear, d.getDayOfMonth)))("year", "month", "day")
  }

  implicit def projectStatusCodec: CodecJson[ProjectStatus] = casecodec2(ProjectStatus.apply, ProjectStatus.unapply)("date", "epics")
}