package tracking

import org.joda.time.LocalDate

import argonaut.Argonaut.ToJsonIdentity
import tracking.json.projectStatusCodec
import tracking.model.{Dependency, Epic, EpicComposition, IdentifierAndTitle, InProgress, NotStarted, ProjectStatus}

object StatusWriter extends App {
  val status = ProjectStatus(
    new LocalDate(2014, 12, 1),
    List(
      Epic(IdentifierAndTitle("id1", "title1"), NotStarted, None, Nil),
      Epic(IdentifierAndTitle("id2", "title2"), NotStarted, None, List(Dependency("p2", "p2-epic"), Dependency("p3", "p3-epic"))),
      Epic(IdentifierAndTitle("id3", "title3"), InProgress, Some(EpicComposition(1, 2, 3)), Nil)
    )
  )
  
  println(status.asJson.spaces2)
}