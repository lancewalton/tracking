package tracking

import java.io.{ File, FileWriter }
import scala.xml.NodeSeq
import scalaz.NonEmptyList
import tracking.model.{ Epic, Project, ProjectStatus }
import tracking.repository.{ ProblemLoadingProjectStatusFile, ProjectDirectoryNameParseFailure, RepositoryLoadError, RepositoryLoader }
import tracking.model.Repository
import tracking.model.DuplicateProjectName
import tracking.model.EmptyProjectName
import tracking.model.DuplicateProjectId
import tracking.model.EmptyProjectId
import tracking.model.DuplicateReportStatusDate
import tracking.model.EmptyEpicTitle
import scalaz.Show
import org.joda.time.LocalDate
import tracking.model.DuplicateEpicTitle
import tracking.model.EmptyEpicId
import tracking.model.DuplicateEpicId
import tracking.model.DependencyRefersToUnknownProject

object Tracking extends App {
  val repository = for {
    projects <- RepositoryLoader(new File("data"))
    repository <- Repository.createValid(projects)
  } yield repository
  
  repository
    .bimap(reportErrors(_), generateReport(_))
    .foreach { saveReport(_) }

  private def reportErrors(errors: NonEmptyList[_]) {
    import scalaz.syntax.show._
    
    implicit val localDateShow = new Show[LocalDate] {
      override def shows(d: LocalDate) = s"${d.getDayOfMonth}/${d.getMonthOfYear}/${d.getYear}"
    }
    
    println("Can't produce the report because of errors while loading the data:")
    errors.map {
      _ match {
        case ProjectDirectoryNameParseFailure(directory)       => s"Can't parse project directory name '${directory.getName}' found at ${directory.getAbsolutePath}. Need <name> or <name>.<id>"
        case ProblemLoadingProjectStatusFile(file, parseError) => s"Can't load project status file ${file.getAbsolutePath}: $parseError"
        case DuplicateProjectName(name) => s"Duplicate project name: '$name'"
        case EmptyProjectName => "Empty project name"
        case DuplicateProjectId(id) => s"Duplicate project id: '$id'"
        case EmptyProjectId => "Empty project id"
        case DuplicateReportStatusDate(project, date) => s"Duplicate report status date in project ${project.identifiers.show}: ${date.show}"
        case EmptyEpicTitle(project, status, epic) => s"Empty epic title in project ${project.identifiers.show}, status ${status.date.show}: ${epic.show}"
        case DuplicateEpicTitle(project, status, title) => s"Duplicate epic title in project ${project.identifiers.show}, status ${status.date.show}: '$title'"
        case EmptyEpicId(project, status, epic) => s"Empty epic id in project ${project.identifiers.show}, status ${status.date.show}: ${epic.show}"
        case DuplicateEpicId(project, status, id) => s"Duplicate epic title in project ${project.identifiers.show}, status ${status.date.show}: '$id'"
        case DependencyRefersToUnknownProject(project, status, dependency) => s"Dependency ${dependency.show} in status ${status.date.show} of project ${project.identifiers.show} refers to non-existent project id '${dependency.projectId}'"
        case e => s"Unkown error: $e"
      }
    }.foreach { msg => println(s"\t$msg") }
  }
  
  private def saveReport(report: NodeSeq) {
    val writer = new FileWriter("report.html")
    writer.write(report.toString)
    writer.close
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
        <h1>BOOST Status</h1>
        { repository.projects.sortBy(_.identifiers.title).map(renderProject) }
      </body>
    </html>
  }

  private def renderProject(project: Project): NodeSeq = {
    <div>{
      renderTitle(project.identifiers.title) ++ renderReportBody(project)
    }</div>
  }

  private def renderReportBody(project: Project) = {
    if (project.statuses.isEmpty) <h2>No data</h2>
    else {
      val sortedStatuses = project.statuses.sorted
      sortedStatuses.reverse.headOption.map(renderStatus(_)).getOrElse(NodeSeq.Empty) ++
        Burndown(sortedStatuses)
    }
  }

  private def renderStatus(status: ProjectStatus) =
    renderEpicProgressBar(status) ++
      <div class="all-epics">
        <div class="completed-epics-list">{ renderCompletedEpics(status) }</div>
        <div class="incomplete-epics-list">{ renderIncompleteEpics(status) }</div>
        <div class="unstarted-epics-list">{ renderNotStartedEpics(status) }</div>
      </div>

  private def renderTitle(title: String) = <h2>{ title }</h2>

  private def renderEpicProgressBar(status: ProjectStatus) =
    renderProgressBar(status.completedEpics.size, status.epicsInProgress.size, status.unstartedEpics.size)

  private def renderProgressBar(completed: Int, inProgress: Int, notStarted: Int) = {
    val total = completed + inProgress + notStarted
    val completedPercentage = percentage(completed, total)
    val inProgressPercentage = percentage(inProgress, total)
    val unstartedPercentage = 100 - completedPercentage - inProgressPercentage

    <div class="progress">
      <div class="progress-bar progress-bar-success" style={ s"width: ${completedPercentage}%" }>
        <span>Completed</span>
      </div>
      <div class="progress-bar progress-bar-warning progress-bar" style={ s"width: ${inProgressPercentage}%}" }>
        <span>In Progress</span>
      </div>
      <div class="progress-bar progress-bar-danger" style={ s"width: ${unstartedPercentage}%" }>
        <span>Not Started</span>
      </div>
    </div>
  }

  private def epicTable(name: String, epics: List[Epic]) =
    if (epics.isEmpty) NodeSeq.Empty
    else <h3>{ name }</h3> ++ { epics.map { epic => <div>{ epic.title }</div> } }

  private def renderCompletedEpics(status: ProjectStatus) = epicTable("Completed Epics", status.completedEpics)

  private def renderNotStartedEpics(status: ProjectStatus) = epicTable("Unstarted Epics", status.unstartedEpics)

  private def renderIncompleteEpics(status: ProjectStatus) =
    if (status.epicsInProgress.isEmpty) NodeSeq.Empty
    else
      <h3>Epics In Progress</h3> ++ {
        status.epicsInProgress.map { epic =>
          <div class="incomplete-epic-row">
            <div class="in-progress-epic-name">{ epic.epic.title }</div>
            <div class="in-progress-epic-completion">{ renderProgressBar(epic.completedStories, epic.storiesInProgress, epic.unstartedStories) }</div>
          </div>
        }
      }

  private def percentage(numerator: Int, denominator: Int): Int = (numerator * 100) / denominator
}