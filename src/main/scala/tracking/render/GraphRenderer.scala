package tracking.render

import tracking.model.Repository
import tracking.model.Project
import org.joda.time.LocalDate
import tracking.model.Complete
import tracking.model.InProgress
import tracking.model.NotStarted
import scalaz.syntax.equal._
import tracking.model.EpicStatus
import tracking.model.Epic
import scala.xml.NodeSeq
import scala.xml.Elem
import java.text.NumberFormat
import java.text.DecimalFormat

case class Location(x: Int, y: Int)
case class Node(id: String, name: String, description: String, centre: Location)
case class Line(from: Node, to: Node) {
	def fromX = from.centre.x
	def fromY = from.centre.y
  def toX = to.centre.x
	def toY = to.centre.y
}
case class PieChartData(complete: Int, inProgress: Int, notStarted: Int) {
  val total = complete + inProgress + notStarted
}

object GraphRenderer {
  def apply(repository: Repository, project: Project, date: LocalDate) = {
    val statusByProject =
      repository
        .reachableEpics(project, date)
        .groupBy(_._1)
        .map { case (p, pe) => (p, pe.map(_._2)) }
        .map { case (p, es) => (Project.idLens.get(p), PieChartData(countEpicsWithStatus(es, Complete), countEpicsWithStatus(es, InProgress), countEpicsWithStatus(es, NotStarted))) }
    
    val selfServe = Node("Self-Serve", "Self-Serve", "", Location(150, 150))
    val durian = Node("risk", "Durian", "Risk", Location(250,100))
    val acerola = Node("ao", "Acerola", "Account Opening", Location(250, 150))
    val kumquat = Node("qcc", "Kumquat", "QCC", Location(250, 200))
    val rhubarb = Node("scrn", "Rhubarb", "Screening", Location(250, 250))
    val fig = Node("faac", "Fig", "Ful. API Anti-Corruption",Location(350, 150))
    val rambutan = Node("cc", "Rambutan", "Country Classification", Location(350, 100))
    
    val nodes = Set(selfServe, durian, acerola, kumquat, fig, rhubarb, rambutan)
    val lines = Set(Line(selfServe, acerola), Line(selfServe, durian), Line(selfServe, kumquat), Line(selfServe, rhubarb), Line(durian, rambutan), Line(acerola, fig))
    
    <svg class="architecture">
      { lines.map { l => <line x1={l.fromX.toString} y1={l.fromY.toString} x2={l.toX.toString} y2={l.toY.toString}/> } }

      { nodes.map { n =>
        <g transform={s"translate(${n.centre.x},${n.centre.y})"}>
          { pieChart(statusByProject.get(n.id)) }
          <text x="20" dy="-1em">{n.name}</text>
          <text x="20" dy="1.35em">{n.description}</text>
        </g>
      } }
    </svg>
  }
  
  private def pieChart(data: Option[PieChartData]) =
    data map { d =>
      pieChartSegment(0, d.notStarted, d.total, "not-started") ++
      pieChartSegment(d.notStarted, d.inProgress, d.total, "in-progress") ++
      pieChartSegment(d.notStarted + d.inProgress, d.complete, d.total, "complete")
    } getOrElse <circle r="15"/>
  
  private def pieChartSegment(startAmount: Int, amount: Int, total: Int, cssClass: String): NodeSeq =
    if (amount == 0) NodeSeq.Empty
    else <path class={s"pie-chart-segment $cssClass"} d={describeArc(15, startAmount, startAmount + amount, total)}/>
  
  private def polarToCartesian(radius: Double, angleInDegrees: Double) = {
    val angleInRadians = angleInDegrees * Math.PI / 180.0
    (radius * Math.cos(angleInRadians), radius * Math.sin(angleInRadians))
  }

  private def describeArc(radius: Double, startAmount: Int, endAmount: Int, total: Int) = {
    val startDegrees = (startAmount * 360) / total
    val endDegrees = (endAmount * 360) / total
    
    val start = polarToCartesian(radius, startDegrees)
    val end = polarToCartesian(radius, endDegrees)
    
    val largeArc = if (endDegrees - startDegrees > 180) "1" else "0"
      
    val format = new DecimalFormat("#.###")
    s"M 0,0 L ${format.format(start._1)},${format.format(start._2)} A $radius,$radius 0 $largeArc 1 ${format.format(end._1)},${format.format(end._2)} Z"
  }
  
  private def countEpicsWithStatus(epics: Iterable[Epic], status: EpicStatus) = epics.filter(_.status === status).size
}