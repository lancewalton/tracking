package tracking

import java.io.{ File, FileWriter }

import scala.xml.{ Elem, NodeSeq }
import scala.xml.NodeSeq.seqToNodeSeq

import scalaz.{ NonEmptyList, Show, Validation }
import scalaz.Validation.FlatMap.ValidationFlatMapRequested
import scalaz.syntax.show.ToShowOps
import scalaz.syntax.std.list.ToListOpsFromList
import scalaz.syntax.std.option.ToOptionOpsFromOption

import org.joda.time.LocalDate

import tracking.model.{ Complete, DependencyRefersToUnknownEpic, DependencyRefersToUnknownProject, DuplicateEpicId, DuplicateEpicTitle, DuplicateProjectId, DuplicateProjectName, DuplicateReportStatusDate, EmptyEpicId, EmptyEpicTitle, EmptyProjectId, EmptyProjectName, Epic, InProgress, NotStarted, Project, ProjectStatus, Repository }
import tracking.repository.{ ProblemLoadingProjectStatusFile, ProjectDirectoryNameParseFailure, RepositoryLoader }

object Tracking extends App {
  val repository: Validation[NonEmptyList[Object], Repository] =
    for {
      projects <- RepositoryLoader(new File("data"))
      repository <- Repository.createValid(projects)
    } yield repository

  repository bimap (reportErrors, generateReport) foreach saveReport

  private def reportErrors(errors: NonEmptyList[_]): Unit = {
    import scalaz.syntax.show._

    implicit val localDateShow = new Show[LocalDate] {
      override def shows(d: LocalDate) = s"${d.getDayOfMonth}/${d.getMonthOfYear}/${d.getYear}"
    }

    println("Can't produce the report because of errors while loading the data:")
    errors.map {
      case ProjectDirectoryNameParseFailure(directory) => s"Can't parse project directory name '${directory.getName}' found at ${directory.getAbsolutePath}. Need <name> or <name>.<id>"
      case ProblemLoadingProjectStatusFile(file, parseError) => s"Can't load project status file ${file.getAbsolutePath}: $parseError"
      case DuplicateProjectName(name) => s"Duplicate project name: '$name'"
      case EmptyProjectName => "Empty project name"
      case DuplicateProjectId(id) => s"Duplicate project id: '$id'"
      case EmptyProjectId => "Empty project id"
      case DuplicateReportStatusDate(project, date) => s"Duplicate report status date in project ${project.identifiers.show}: ${date.show}"
      case EmptyEpicTitle(project, status, epic) => s"Empty epic title in project ${project.identifiers.show}, status ${status.date.show}: ${epic.identifiers.show}"
      case DuplicateEpicTitle(project, status, title) => s"Duplicate epic title in project ${project.identifiers.show}, status ${status.date.show}: '$title'"
      case EmptyEpicId(project, status, epic) => s"Empty epic id in project ${project.identifiers.show}, status ${status.date.show}: ${epic.identifiers.show}"
      case DuplicateEpicId(project, status, id) => s"Duplicate epic title in project ${project.identifiers.show}, status ${status.date.show}: '$id'"
      case DependencyRefersToUnknownProject(project, status, dependency) => s"Dependency ${dependency.show} in status ${status.date.show} of project ${project.identifiers.show} refers to non-existent project id '${dependency.projectId}'"
      case DependencyRefersToUnknownEpic(project, status, dependency) => s"Dependency ${dependency.show} in status ${status.date.show} of project ${project.identifiers.show} refers to non-existent epic id '${dependency.epicId}'. The referenced project was found, but it does not contain the referenced epic."
      case e => s"Unknown error: $e"
    }.foreach { msg => println(s"\t$msg") }
  }

  private def saveReport(report: NodeSeq): Unit = {
    val writer = new FileWriter("report.html")
    writer.write(report.toString())
    writer.close()
  }

  private def generateReport(repository: Repository): NodeSeq = {
    <html>
      <head>
        <link href="libs/bootstrap/css/bootstrap.min.css" rel="stylesheet" type="text/css"/>
        <link href="css/tracking.css" rel="stylesheet" type="text/css"/>
        <script type="text/javascript" src="libs/amcharts/amcharts.js"></script>
        <script type="text/javascript" src="libs/amcharts/serial.js"></script>
        <script type="text/javascript" src="libs/amcharts/themes/chalk.js"></script>
      </head>
      <body>
        <h1>BOOST Status</h1>{ renderProjects(repository) }
      </body>
    </html>
  }

  private def renderProjects(repository: Repository): NodeSeq =
    repository.projects.sortBy(_.identifiers.title).map(renderProject(repository, _))

  private def renderProject(repository: Repository, project: Project): Elem =
    <div>
      { renderProjectBody(repository, project) }
    </div>

  private def renderProjectBody(repository: Repository, project: Project): NodeSeq =
    renderTitle(project.identifiers.title) ++ renderReportBody(project)

  private def renderReportBody(project: Project): NodeSeq =
    project.statuses.toNel.cata(renderReportBody, <h2>No data</h2>)

  private def renderReportBody(statuses: NonEmptyList[ProjectStatus]): NodeSeq = {
    val sortedStatuses = statuses.sorted.reverse
    renderStatus(sortedStatuses.head) ++ Burndown(sortedStatuses)
  }

  private def renderStatus(status: ProjectStatus): NodeSeq =
    renderEpicProgressBar(status) ++
      <div class="all-epics">
        <div class="completed-epics-list">
          { renderCompletedEpics(status) }
        </div>
        <div class="incomplete-epics-list">
          { renderIncompleteEpics(status) }
        </div>
        <div class="unstarted-epics-list">
          { renderNotStartedEpics(status) }
        </div>
      </div>

  private def renderTitle(title: String): Elem = <h2>
                                                   { title }
                                                 </h2>

  private def renderEpicProgressBar(status: ProjectStatus) =
    renderProgressBar(status.epicsWithStatus(Complete).size, status.epicsWithStatus(InProgress).size, status.epicsWithStatus(NotStarted).size)

  private def renderProgressBar(completed: Int, inProgress: Int, notStarted: Int) = {
    val total = completed + inProgress + notStarted
    val completedPercentage = percentage(completed, total)
    val inProgressPercentage = percentage(inProgress, total)
    val unstartedPercentage = 100 - completedPercentage - inProgressPercentage

    <div class="progress">
      <div class="progress-bar progress-bar-success" style={ s"width: $completedPercentage%" }>
        <span>Completed</span>
      </div>
      <div class="progress-bar progress-bar-warning progress-bar" style={ s"width: $inProgressPercentage%}" }>
        <span>In Progress</span>
      </div>
      <div class="progress-bar progress-bar-danger" style={ s"width: $unstartedPercentage%" }>
        <span>Not Started</span>
      </div>
    </div>
  }

  private def epicTable(name: String, epics: List[Epic]): NodeSeq =
    epics.toNel.cata(epicTable(name, _), NodeSeq.Empty)

  private def epicTable(name: String, epics: NonEmptyList[Epic]): NodeSeq =
    <h3>
      { name }
    </h3> ++ {
      epics.list.map { epic =>
        <div>
          { epic.identifiers.title }
        </div>
      }
    }

  private def renderCompletedEpics(status: ProjectStatus) = epicTable("Completed Epics", status.epicsWithStatus(Complete))

  private def renderNotStartedEpics(status: ProjectStatus) = epicTable("Unstarted Epics", status.epicsWithStatus(NotStarted))

  private def renderIncompleteEpics(status: ProjectStatus): NodeSeq =
    status.epicsWithStatus(InProgress).toNel.cata(renderIncompleteEpics, NodeSeq.Empty)

  private def renderIncompleteEpics(status: NonEmptyList[Epic]): NodeSeq =
    <h3>Epics In Progress</h3> ++ {
      status.map { epic =>
        <div class="incomplete-epic-row">
          <div class="in-progress-epic-name">
            { epic.identifiers.title }
          </div>
          <div class="in-progress-epic-completion">
            { renderProgressBar(epic.composition.fold(0)(_.completedStories), epic.composition.fold(1)(_.storiesInProgress), epic.composition.fold(0)(_.unstartedStories)) }
          </div>
        </div>
      }
    }.list

  private def percentage(numerator: Int, denominator: Int): Int = (numerator * 100) / denominator
}