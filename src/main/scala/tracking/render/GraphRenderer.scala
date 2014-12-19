package tracking.render

case class Location(x: Int, y: Int)
case class Node(id: String, name: String, description: String, centre: Location)
case class Line(from: Node, to: Node) {
	def fromX = from.centre.x
	def fromY = from.centre.y
  def toX = to.centre.x
	def toY = to.centre.y
}

object GraphRenderer {
  def apply() = {
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
          <circle r="15"/>
          <text x="15" dy="-1em">{n.name}</text>
          <text x="15" dy="1.35em">{n.description}</text>
        </g>
      } }
    </svg>
  }
}