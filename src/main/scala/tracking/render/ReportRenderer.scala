package tracking.render

import scala.xml.{Elem, NodeSeq}
import scala.xml.NodeSeq.seqToNodeSeq

import scalaz.NonEmptyList
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._

import tracking.model.{Complete, Epic, InProgress, NotStarted, Project, ProjectStatus, Repository}

case class ReportRenderer(repository: Repository) {
  def render: NodeSeq = {
    <html>
      <head>
        <link href="libs/bootstrap/css/bootstrap.min.css" rel="stylesheet" type="text/css"/>
        <link href="css/tracking.css" rel="stylesheet" type="text/css"/>
        <script type="text/javascript" src="libs/amcharts/amcharts.js"></script>
        <script type="text/javascript" src="libs/amcharts/serial.js"></script>
        <script type="text/javascript" src="libs/amcharts/themes/chalk.js"></script>
      </head>
      <body>
        <h1>BOOST Status</h1>{ renderProjects }
      </body>
    </html>
  }

  private def renderProjects: NodeSeq =
    repository.projects.sortBy(_.identifiers.title).map(renderProject(repository, _))

  private def renderProject(repository: Repository, project: Project): Elem =
    <div>
      { renderProjectBody(project) }
    </div>

  private def renderProjectBody(project: Project): NodeSeq =
    renderTitle(project.identifiers.title) ++ renderReportBody(project)

  private def renderReportBody(project: Project): NodeSeq =
    project.statuses.toNel.cata(renderReportBody, <h2>No data</h2>)

  private def renderReportBody(statuses: NonEmptyList[ProjectStatus]): NodeSeq = {
    val sortedStatuses = statuses.sorted.reverse
    renderStatus(sortedStatuses.head) ++ BurndownRenderer(sortedStatuses)
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
    ProgressBarRenderer(status.epicsWithStatus(Complete).size, status.epicsWithStatus(InProgress).size, status.epicsWithStatus(NotStarted).size)

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
            { ProgressBarRenderer(epic.composition.fold(0)(_.completedStories), epic.composition.fold(1)(_.storiesInProgress), epic.composition.fold(0)(_.unstartedStories)) }
          </div>
        </div>
      }
    }.list
}