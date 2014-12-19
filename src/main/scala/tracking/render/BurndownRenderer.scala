package tracking.render

import java.text.DecimalFormat
import java.util.UUID

import scala.annotation.tailrec
import scala.xml.{ NodeSeq, Unparsed }

import scalaz.NonEmptyList
import scalaz.syntax.equal.ToEqualOps
import scalaz.syntax.std.list.ToListOpsFromList
import scalaz.syntax.std.option.ToOptionOpsFromOption

import org.joda.time.LocalDate

import tracking.model.{ Complete, Project, Repository }

case class BurndownDatum(date: LocalDate, epicsToDo: Int)

case object BurndownRenderer {
  type BurndownData = List[BurndownDatum]

  def apply(repository: Repository, project: Project) = eliminateDuplicates(makeData(repository, project)).toNel.cata(drawChart(_), NodeSeq.Empty)

  private def makeData(repository: Repository, project: Project): BurndownData =
    repository.dates.sortWith((x, y) => x.isBefore(y))
      .map { date => BurndownDatum(date, repository.reachableEpics(project, date).toList.filter(_._2.status =/= Complete).size) }

  private def drawChart(data: NonEmptyList[BurndownDatum]) = {
    val id = UUID.randomUUID.toString
    <div id={ id } class="burndown"/> ++ <script>{ new Unparsed(makeDatesChart(id, data)) }</script>
  }

  private def eliminateDuplicates(data: BurndownData) = {
    @tailrec
    def recurse(acc: BurndownData, remainingData: BurndownData, previousUnemitted: Option[BurndownDatum]): BurndownData =
      (acc, remainingData, previousUnemitted) match {
        // No more data to accumulate and no previously unaccumulated value. Return what we've accumulated.
        case (result, Nil, None) => result.reverse

        // No more data to accumulate but there is a previously unaccumulated value. Prepend the unaccumulated value and recurse. No unaccumulated value to carry forward.
        case (result, Nil, Some(unaccumulated)) => recurse(unaccumulated :: result, Nil, None)

        // The first value to accumulate. Accumulate it and recurse. No unaccumulated data to carry forward.
        case (Nil, h :: t, _) => recurse(h :: Nil, t, None)

        // Same value as the last one. Recurse without accumulating and keep hold of the unaccumulated value in case we need it later.
        case (ah :: at, rh :: rt, _) if (ah.epicsToDo == rh.epicsToDo) => recurse(ah :: at, rt, Some(rh))

        // Not the same value as the last one. Accumulate it and recurse with no unaccumulated value.

        case (ah :: at, rh :: rt, None) => recurse(rh :: ah :: at, rt, None)

        // Not the same value as the last one and we also have an unaccumulated value.
        // Accumulate the unaccumulated value and the latest value and recurse with no unaccumulated value.
        case (ah :: at, rh :: rt, Some(unaccumulated)) => recurse(rh :: unaccumulated :: ah :: at, rt, None)
      }

    recurse(Nil, data, None)
  }

  private def makeDatesChart(id: String, data: NonEmptyList[BurndownDatum]) =
    s"""|var chart = AmCharts.makeChart("$id", {
       |  "type": "serial",
       |  "theme": "none",
       |  "pathToImages": "libs/amcharts/images/",
       |  "dataDateFormat": "YYYY-MM-DD",
       |  "dataProvider": [${dateData(data)}],
       |  "valueAxes": [{
       |    "gridColor":"#FFFFFF",
       |    "gridAlpha": 0.2,
       |    "dashLength": 0
       |  }],
       |  "gridAboveGraphs": true,
       |  "startDuration": 1,
       |  "graphs": [{
       |    "id": "g1",
       |    "title": "Burndown",
       |    "balloonText": "[[title]]: <b>[[value]]</b>",
       |    "bullet": "round",
       |    "bulletSize": 10,
       |    "bulletBorderColor": "#ffffff",
       |    "bulletBorderAlpha": 1,
       |    "bulletBorderThickness": 2,
       |    "valueField": "burndown"
       |  }],
       | "chartScrollbar": {
       |   "graph": "g1",
       |   "scrollbarHeight": 30
       |  },
       |  "chartCursor": {
       |    "categoryBalloonEnabled": false,
       |    "cursorAlpha": 0,
       |    "zoomable": false
       |  },
       |  "categoryField": "date",
       |  "categoryAxis": {
       |    "parseDates": true,
       |    "dashLength": 1,
       |    "minorGridEnabled": true,
       |    "position": "top"
       |  },
       |  legend: {}
       |});""".stripMargin

  private def dateData(data: NonEmptyList[BurndownDatum]): String = {
    val rows = for {
      datum <- data
    } yield s"""|{
               |  "date": "${datum.date.getYear}-${twoDigits(datum.date.getMonthOfYear)}-${twoDigits(datum.date.getDayOfMonth)}",
               |  "burndown": ${datum.epicsToDo},
            |}"""

    rows.list.mkString(",\n").stripMargin
  }

  private def twoDigits(n: Number) = new DecimalFormat("00").format(n)
}