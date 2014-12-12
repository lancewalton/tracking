package tracking

import scalaz.ValidationNel
import org.joda.time.LocalDate
import argonaut.Argonaut.{ IntDecodeJson, IntEncodeJson, ListDecodeJson, ListEncodeJson, StringDecodeJson, StringEncodeJson, casecodec2, casecodec3, casecodec4, casecodec5 }
import argonaut.CodecJson
import tracking.model.{ Dependency, Epic, EpicWithStories, ProjectStatus }
import tracking.repository.RepositoryLoadError
import tracking.model.IdentifierAndTitle

package object repository {
  implicit def identifierWithTitleCodec = casecodec2(IdentifierAndTitle.apply, IdentifierAndTitle.unapply)("id", "title")

  implicit def epicWithStoriesCodec =
    casecodec4(EpicWithStories.apply, EpicWithStories.unapply)("epic", "completed_stories", "unstarted_stories", "stories_in_progress")

  implicit def localDateCodec: CodecJson[LocalDate] = {
    casecodec3[Int, Int, Int, LocalDate](
      new LocalDate(_, _, _),
      (d: LocalDate) => Option((d.getYear, d.getMonthOfYear, d.getDayOfMonth)))("year", "month", "day")
  }

  implicit def dependencyCodec = casecodec2(Dependency.apply, Dependency.unapply)("project", "epicId")

  implicit def projectStatusCodec: CodecJson[ProjectStatus] =
    casecodec5(ProjectStatus.apply, ProjectStatus.unapply)("date", "completed_epics", "unstarted_epics", "epics_in_progress", "dependencies")

  type LoadedRepository[T] = ValidationNel[RepositoryLoadError, T]
}