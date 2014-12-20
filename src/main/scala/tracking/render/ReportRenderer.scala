package tracking.render

import scala.annotation.migration
import scala.xml.{ Elem, NodeSeq }
import scala.xml.NodeSeq.seqToNodeSeq

import scalaz.NonEmptyList
import scalaz.syntax.equal.ToEqualOps
import scalaz.syntax.std.list.ToListOpsFromList
import scalaz.syntax.std.option.ToOptionOpsFromOption

import org.joda.time.LocalDate

import tracking.model.{ Complete, Epic, EpicStatus, InProgress, NotStarted, Project, Repository }

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
    repository.projects.sortBy(Project.titleLens.get).map(renderProject(_))

  private def renderProject(project: Project): Elem =
    <div>
      { renderProjectBody(project) }
    </div>

  private def renderProjectBody(project: Project): NodeSeq =
    renderTitle(Project.titleLens.get(project)) ++ renderReportBody(project)

  private def renderReportBody(project: Project): NodeSeq =
    repository.dates.toNel.cata(renderReportBody(project, _), <h2>No data</h2>)

  private def renderReportBody(project: Project, dates: NonEmptyList[LocalDate]): NodeSeq = {
    val sortedDates = dates.sortWith((x, y) => x.isBefore(y)).reverse
    val latestDate = sortedDates.head
    renderStatus(project, latestDate) ++ <div class="charts">{GraphRenderer(repository, project, latestDate)}{BurndownRenderer(repository, project)}</div>
  }

  private def renderStatus(project: Project, date: LocalDate): NodeSeq = {
    val epics = repository.reachableEpics(project, date)
    renderEpicProgressBar(epics.map(_._2)) ++
      <div class="all-epics">
        <div class="completed-epics-list">
          { renderCompletedEpics(epics) }
        </div>
        <div class="incomplete-epics-list">
          { renderIncompleteEpics(epics) }
        </div>
        <div class="unstarted-epics-list">
          { renderNotStartedEpics(epics) }
        </div>
      </div>
  }

  private def renderTitle(title: String): Elem = <h2>
                                                   { title }
                                                 </h2>

  private def renderEpicProgressBar(epics: Set[Epic]) =
    ProgressBarRenderer(epics.filter(_.status === Complete).size, epics.filter(_.status === InProgress).size, epics.filter(_.status === NotStarted).size)

  private def epicTable(name: String, epics: Set[(Project, Epic)], status: EpicStatus): NodeSeq =
    <h3>
      { name }
    </h3> ++
      epics.filter(_._2.status === status).toList.toNel.cata(epicTable(name, _), <div>None</div>)

  private def epicTable(name: String, epics: NonEmptyList[(Project, Epic)]): NodeSeq =
    epics.list.map { case (p, e) => (Project.titleLens.get(p), e.identifiers.title) }.sorted.map {
      case (p, e) =>
        <div>{ p } : { e }</div>
    }

  private def renderCompletedEpics(epics: Set[(Project, Epic)]) = epicTable("Completed Epics", epics, Complete)

  private def renderNotStartedEpics(epics: Set[(Project, Epic)]) = epicTable("Unstarted Epics", epics, NotStarted)

  private def renderIncompleteEpics(epics: Set[(Project, Epic)]): NodeSeq =
    <h3>
      Epics In Progress
    </h3> ++
      epics.filter(_._2.status === InProgress).toList.toNel.cata(renderIncompleteEpics, <div>None</div>)

  private def renderIncompleteEpics(epics: NonEmptyList[(Project, Epic)]): NodeSeq =
    epics.map {
      case (project, epic) =>
        <div class="incomplete-epic-row">
          <div class="in-progress-epic-name">{ Project.titleLens.get(project) } : { epic.identifiers.title }</div>
          <div class="in-progress-epic-completion">
            { ProgressBarRenderer(epic.composition.fold(0)(_.completedStories), epic.composition.fold(1)(_.storiesInProgress), epic.composition.fold(0)(_.unstartedStories)) }
          </div>
        </div>
    }.list
}