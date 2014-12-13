
package tracking

import java.text.DecimalFormat
import java.util.UUID
import scala.xml.Unparsed
import tracking.model.ProjectStatus
import tracking.model.NotStarted
import tracking.model.InProgress

import scalaz.NonEmptyList

case object Burndown {
  def apply(data: NonEmptyList[ProjectStatus]) = {
    val id = UUID.randomUUID.toString
    <div id={id} class="burndown"/> ++ <script>{new Unparsed(makeDatesChart(id, data))}</script>
  }

  private def makeDatesChart(id: String, data: NonEmptyList[ProjectStatus]) =
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
        
  private def dateData(data: NonEmptyList[ProjectStatus]): String = {
    val sortedData = data.sorted
    
    val rows = for {
      status <- sortedData
    } yield s"""|{
               |  "date": "${status.date.getYear}-${twoDigits(status.date.getMonthOfYear)}-${twoDigits(status.date.getDayOfMonth)}",
               |  "burndown": ${status.epicsWithStatus(NotStarted).size + status.epicsWithStatus(InProgress).size},
            |}"""
               
    rows.list.mkString(",\n").stripMargin
  }
       
  private def twoDigits(n: Number) = new DecimalFormat("00").format(n)
}