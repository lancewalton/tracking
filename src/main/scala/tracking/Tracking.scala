package tracking

import java.io.{File, FileWriter}
import scala.xml.NodeSeq
import scalaz.{NonEmptyList, Show, Validation}
import scalaz.Validation.FlatMap.ValidationFlatMapRequested
import scalaz.syntax.show._
import org.joda.time.LocalDate
import tracking.model.{DependencyRefersToUnknownEpic, DependencyRefersToUnknownProject, DuplicateEpicId, DuplicateEpicTitle, DuplicateProjectId, DuplicateProjectName, DuplicateReportStatusDate, EmptyEpicId, EmptyEpicTitle, EmptyProjectId, EmptyProjectName, Repository}
import tracking.render.ReportRenderer
import tracking.repository.{ProblemLoadingProjectStatusFile, RepositoryLoader}
import tracking.repository.ProblemLoadingMetaFile

object Tracking extends App {
  val repository: Validation[NonEmptyList[Object], Repository] =
    for {
      projects <- RepositoryLoader(new File("data"))
      repository <- Repository.createValid(projects)
    } yield repository

  repository bimap (reportErrors, ReportRenderer(_).render) foreach saveReport

  private def reportErrors(errors: NonEmptyList[_]): Unit = {
    import scalaz.syntax.show._

    implicit val localDateShow = new Show[LocalDate] {
      override def shows(d: LocalDate) = s"${d.getDayOfMonth}/${d.getMonthOfYear}/${d.getYear}"
    }

    println("Can't produce the report because of errors while loading the data:")
    errors.map {
      case ProblemLoadingMetaFile(file, parseError) => s"Can't load project meta.json file ${file.getAbsolutePath}: $parseError"
      case ProblemLoadingProjectStatusFile(file, parseError) => s"Can't load project status file ${file.getAbsolutePath}: $parseError"
      case DuplicateProjectName(name) => s"Duplicate project name: '$name'"
      case EmptyProjectName => "Empty project name"
      case DuplicateProjectId(id) => s"Duplicate project id: '$id'"
      case EmptyProjectId => "Empty project id"
      case DuplicateReportStatusDate(project, date) => s"Duplicate report status date in project ${project.meta.identifiers.show}: ${date.show}"
      case EmptyEpicTitle(project, status, epic) => s"Empty epic title in project ${project.meta.identifiers.show}, status ${status.date.show}: ${epic.identifiers.show}"
      case DuplicateEpicTitle(project, status, title) => s"Duplicate epic title in project ${project.meta.identifiers.show}, status ${status.date.show}: '$title'"
      case EmptyEpicId(project, status, epic) => s"Empty epic id in project ${project.meta.identifiers.show}, status ${status.date.show}: ${epic.identifiers.show}"
      case DuplicateEpicId(project, status, id) => s"Duplicate epic title in project ${project.meta.identifiers.show}, status ${status.date.show}: '$id'"
      case DependencyRefersToUnknownProject(project, status, epic, dependency) => s"Dependency ${dependency.show} from epic ${epic.identifiers.show} in status ${status.date.show} of project ${project.meta.identifiers.show} refers to non-existent project id '${dependency.projectId}'"
      case DependencyRefersToUnknownEpic(project, status, epic, dependency) => s"Dependency ${dependency.show} from epic ${epic.identifiers.show} in status ${status.date.show} of project ${project.meta.identifiers.show} refers to non-existent epic id '${dependency.epicId}'. The referenced project was found, but it does not contain the referenced epic."
      case e => s"Unknown error: $e"
    }.foreach { msg => println(s"\t$msg") }
  }

  private def saveReport(report: NodeSeq): Unit = {
    val writer = new FileWriter("report.html")
    writer.write(report.toString())
    writer.close()
  }
}