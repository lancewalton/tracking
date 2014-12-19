package tracking

import argonaut.Argonaut.ToJsonIdentity
import tracking.json.architectureCodec
import tracking.model.architecture._

object ArchitectureWriter extends App {
    val selfServe = Node("Self-Serve", "Self-Serve", Location(150, 150))
    val durian = Node("risk", "Risk", Location(250,100))
    val acerola = Node("ao", "Account Opening", Location(250, 150))
    val kumquat = Node("qcc", "QCC", Location(250, 200))
    val rhubarb = Node("scrn", "Screening", Location(250, 250))
    val fig = Node("faac", "Ful. API Anti-Corruption",Location(350, 150))
    val rambutan = Node("cc", "Country Classification", Location(350, 100))

    val architecture = Architecture(
      nodes = selfServe :: durian :: acerola :: kumquat :: rhubarb :: fig :: rambutan :: Nil,
      relationships = Relationship(selfServe.id, acerola.id) :: Relationship(selfServe.id, durian.id) :: Relationship(selfServe.id, kumquat.id) :: Relationship(selfServe.id, rhubarb.id) :: Relationship(durian.id, rambutan.id) :: Relationship(acerola.id, fig.id) :: Nil
  )
  
  println(architecture.asJson.spaces2)
}